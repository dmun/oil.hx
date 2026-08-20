(require "util.scm")
(require "entry.scm")

(provide action
         action?
         action-kind
         action-src
         action-dest
         entry->path
         action->string
         order-actions
         validate-actions)

;; A single pending filesystem operation, produced by diffing a parsed oil
;; document against *directory-cache*.
;;
;;   kind - 'create | 'delete | 'move | 'copy
;;   src  - cached entry the action acts on, or #f for 'create
;;   dest  - buffer entry describing the destination, or #f for 'delete
;;
;; A rename is just a 'move whose src and dest share a parent.
(struct action (kind src dest) #:transparent)

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

;; a action can run once its destination name is free and the directory it
;; writes into exists
(define (action-ready? c state)
  (define dst (action-dest-path c))
  (define parent (action-dest-parent c))
  (and
    (or (not dst) (not (simulated-exists? state dst)))
    (or (not parent) (simulated-exists? state parent))))

;; Orders `actions` so each one runs only once it can: the delete or move that
;; frees a name comes before the action reusing it, and a dest directory comes
;; before the actions that move into it.
;;
;; actions that can never become ready (a -> b while b -> a needs a temporary
;; name) keep their original order; `validate-actions` then reports them.
(fun order-actions :: (actions (listof action?) -> (listof action?))
  ;; the first action that can run, followed by the rest in their original order
  (define (take-ready pending state skipped)
    (cond
      [(empty? pending) #f]
      [(action-ready? (car pending) state)
       (cons (car pending) (concat (reverse skipped) (cdr pending)))]
      [else (take-ready (cdr pending) state (cons (car pending) skipped))]))
  (define (loop pending state ordered)
    (if (empty? pending)
      (reverse ordered)
      (let ([picked (take-ready pending state '())])
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
