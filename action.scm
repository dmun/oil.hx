(require "steel/result")

(require "macros.scm")
(require "util.scm")

(require "entry.scm")
(require "cache.scm")

(provide action
         action?
         action-kind
         action-src
         action-dest
         entry->path
         action->string
         entries->actions
         order-actions
         validate-actions
         actions-apply!)

;; A single pending filesystem operation, produced by diffing parsed oil
;; documents against the entry cache.
;;
;;   kind - 'create | 'delete | 'move | 'copy
;;   src  - cached entry the action acts on, or #f for 'create
;;   dest  - buffer entry describing the destination, or #f for 'delete
;;
;; A rename is just a 'move whose src and dest share a parent.
(struct action (kind src dest) #:transparent)

(fun same-place? :: (a entry? -> b entry? -> boolean?)
  (and (equal? (entry-parent a) (entry-parent b))
       (equal? (entry-name a) (entry-name b))))

(define (entry-counts entries)
  (foldl
    (fn (e counts)
      (let ([id (entry-id e)])
        (hash-insert counts id (+ 1 (or (hash-try-get counts id) 0)))))
    (hash)
    entries))

;; IDs that still occur at their original path. Exactly one such occurrence is
;; kept; any additional occurrence of that ID is a copy.
(define (stationary-ids cache entries)
  (transduce entries
    (compose
      (filtering
        (fn (n)
          (let ([src (hash-try-get cache (entry-id n))])
            (and src (same-place? src n)))))
      (mapping entry-id))
    (into-hashset)))

;; Classifies each occurrence explicitly. If the original path survives, one
;; occurrence stays and the rest are copies. Otherwise the last occurrence is
;; the move, so all copies initially precede the source-consuming action.
(define (classify-entries cache entries)
  (define stationary (stationary-ids cache entries))
  (define (loop pending remaining kept actions)
    (if (empty? pending)
      (reverse actions)
      (let* ([n (car pending)]
             [id (entry-id n)]
             [src (hash-try-get cache id)]
             [left (hash-ref remaining id)]
             [rest-counts (hash-insert remaining id (- left 1))]
             [stays? (and src
                       (same-place? src n)
                       (not (hashset-contains? kept id)))]
             [c (cond
                  ;; unknown id: a line the user typed
                  [(not src) (action 'create #f n)]
                  [stays? #f]
                  [(hashset-contains? stationary id) (action 'copy src n)]
                  [(equal? left 1) (action 'move src n)]
                  [else (action 'copy src n)])])
        (loop (cdr pending)
              rest-counts
              (if stays? (hashset-insert kept id) kept)
              (if c (cons c actions) actions)))))
  (loop entries (entry-counts entries) (hashset) '()))

;; Diffs the buffer entries against an immutable cache snapshot. The result is
;; in buffer order, which is not necessarily a safe order to apply.
(fun entries->actions :: (cache hash? -> dirs (listof string?) -> entries (listof entry?) -> (listof action?))
  (define present
    (transduce entries
      (mapping entry-id)
      (into-hashset)))
  (define deletes
    (transduce (hash-values->list cache)
      (compose
        (filtering (fn (src) (member (entry-parent src) dirs)))
        (filtering (fn (src) (not (hashset-contains? present (entry-id src)))))
        (mapping (fn (src) (action 'delete src #f))))
      (into-list)))
  (concat (classify-entries cache entries) deletes))

(fun entry->path :: (e entry? -> string?)
  (string-append (entry-parent e) "/" (entry-name e)))

(fun action-src-path :: (c action? -> (or/c string? false?))
  (if (action-src c) (entry->path (action-src c)) #f))

(fun action-dest-path :: (c action? -> (or/c string? false?))
  (if (action-dest c) (entry->path (action-dest c)) #f))

;; the directory the action writes into, or #f if it only removes something
(fun action-dest-parent :: (c action? -> (or/c string? false?))
  (if (action-dest c) (entry-parent (action-dest c)) #f))

(fun action->string :: (c action? -> string?)
  (case (action-kind c)
    [(create) (string-append "CREATE " (action-dest-path c)
                (if (equal? (entry-type (action-dest c)) 'directory) "/" ""))]
    [(delete) (string-append "DELETE " (action-src-path c))]
    [(move)   (string-append "MOVE   " (action-src-path c) " -> " (action-dest-path c))]
    [(copy)   (string-append "COPY   " (action-src-path c) " -> " (action-dest-path c))]
    [else     (string-append "?????  " (symbol->string (action-kind c)))]))

;; `state` overlays the fs: path -> #t (will exist) / #f (vacated earlier in the batch)
(define (simulated-exists? state path)
  (if (hash-contains? state path) (hash-ref state path) (path-exists? path)))

;; the overlay after `c` has run: its src name is free, its destination taken
(define (action-effect c state)
  (define src (action-src-path c))
  (define dst (action-dest-path c))
  (define vacated
    (if (and src (member (action-kind c) '(move delete)))
      (hash-insert state src #f)
      state))
  (if dst (hash-insert vacated dst #t) vacated))

(define (name-problem e)
  (define n (entry-name e))
  (cond
    [(equal? n "") "empty name"]
    [(string-contains? n "/") (string-append "name contains a slash: " n)]
    [(string-contains? n "\n") (string-append "name contains a newline: " n)]
    [else #f]))

;; everything wrong with `c` if it ran against `state`, as a list of messages
(fun action-problems :: (c action? -> state hash? -> (listof string?))
  (define src (action-src-path c))
  (define dst (action-dest-path c))
  (define bad-name (if (action-dest c) (name-problem (action-dest c)) #f))
  (define parent (action-dest-parent c))
  (filter string?
    (list
      bad-name
      (if (and src (not (simulated-exists? state src)))
        (string-append "src is gone: " src)
        #f)
      (if (and dst (not bad-name) (simulated-exists? state dst))
        (string-append "would overwrite: " dst)
        #f)
      (if (and parent (not bad-name) (not (simulated-exists? state parent)))
        (string-append "no such directory: " parent)
        #f))))

;; True when another pending action still needs to copy `src` before it moves or
;; disappears. Entries have stable IDs, so this remains true across renames.
(define (pending-copy-of? pending src)
  (cond
    [(empty? pending) #f]
    [else
     (define c (car pending))
     (define candidate (action-src c))
     (or (and candidate
              (equal? (action-kind c) 'copy)
              (equal? (entry-id candidate) (entry-id src)))
         (pending-copy-of? (cdr pending) src))]))

;; An action can run once its source is still there, its destination name is
;; free, and the directory it writes into exists. A move/delete also waits for
;; every pending copy that still needs its source.
(define (action-ready? c state pending)
  (define src (action-src-path c))
  (define dst (action-dest-path c))
  (define parent (action-dest-parent c))
  (and
    (or (not src) (simulated-exists? state src))
    (or (not dst) (not (simulated-exists? state dst)))
    (or (not parent) (simulated-exists? state parent))
    (or (not (member (action-kind c) '(move delete)))
        (not (pending-copy-of? pending (action-src c))))))

;; Orders `actions` so each one runs only once it can: the delete or move that
;; frees a name comes before the action reusing it, and a dest directory comes
;; before the actions that move into it.
;;
;; actions that can never become ready (a -> b while b -> a needs a temporary
;; name) keep their original order; `validate-actions` then reports them.
(fun order-actions :: (actions (listof action?) -> (listof action?))
  ;; the first action that can run, followed by the rest in their original order
  (define (take-ready candidates all state skipped)
    (cond
      [(empty? candidates) #f]
      [(action-ready? (car candidates) state all)
       (cons (car candidates) (concat (reverse skipped) (cdr candidates)))]
      [else (take-ready (cdr candidates) all state
              (cons (car candidates) skipped))]))
  (define (loop pending state ordered)
    (if (empty? pending)
      (reverse ordered)
      (let ([picked (take-ready pending pending state '())])
        (if picked
          (loop (cdr picked) (action-effect (car picked) state) (cons (car picked) ordered))
          ;; deadlocked: emit what we have plus the rest, unordered
          (concat (reverse ordered) pending)))))
  (loop actions (hash) '()))

;; empty = safe to apply in this order
(fun validate-actions :: (actions (listof action?) -> (listof string?))
  (define (label c problem)
    (string-append (action->string c) "  !! " problem))
  (define (loop pending state found)
    (if (empty? pending)
      (reverse found)
      (let* ([c (car pending)]
             [problems (map (fn (p) (label c p)) (action-problems c state))])
        (loop (cdr pending)
              (action-effect c state)
              (foldl cons found problems)))))
  (loop actions (hash) '()))

;; #t if `cmd` exited 0; anything else is reported on the status line
(fun run! :: (cmd string? -> args (listof string?) -> (Result/c void? string?))
     (ok-and-then
       (spawn-process (command cmd args))
       (fn (child)
           (ok-and-then
             (wait child)
             (fn (status)
                 (if (equal? status 0)
                   (Ok void)
                   (Err
                     (string-append
                       "oil: " cmd " exited " (number->string status)))))))))

(define (create-path! e)
  (if (directory? e)
    (create-directory! (entry->path e))
    (close-output-port (open-output-file (entry->path e)))))

;; a symlink is unlinked like a file, whatever it points at
(define (delete-path! e)
  (if (directory? e)
    (delete-directory! (entry->path e))
    (delete-file! (entry->path e))))

;; runs one action and mirrors it in the cache, so ids survive moves and no
;; re-read is needed afterwards; #f if the disk operation failed
(fun action-apply! :: (c action? -> (Result/c void? string?))
     (with-handler
       (fn (err) (Err err))
       (let ([src (action-src c)]
             [dest (action-dest c)])
         (case (action-kind c)
           [(create)
            (create-path! dest)
            (cache-add! dest)]
           [(move)
            (rename-file-or-directory! (entry->path src) (entry->path dest))
            (cache-move! src dest)]
           [(delete)
            (delete-path! src)
            (cache-remove! src)]
           [(copy)
            (ok-and-then
              (run! "cp" (list "-a" (entry->path src) (entry->path dest)))
              (fn (_)
                  (cache-add! dest)))]
           [else (error "oil: unknown action " (symbol->string (action-kind c)))]))))

;; stops at the first failure, so the cache never describes a filesystem that
;; isn't there TODO
(fun actions-apply! :: (actions (listof action?) -> (Result/c void? string?))
     (define (loop remaining)
       (if (empty? remaining)
         (Ok void)
         (ok-and-then (action-apply! (car remaining))
                      (fn (_) (loop (cdr remaining))))))
     (loop actions))


