(require "util.scm")
(require "entry.scm")

(provide change
         change?
         change-kind
         change-old
         change-new
         entry->path
         change-source
         change-dest
         change->string
         validate-changes)

;; A single pending filesystem operation, produced by diffing a parsed oil
;; document against *directory-cache*.
;;
;;   kind - 'create | 'delete | 'move | 'copy
;;   old  - cached entry the change acts on, or #false for 'create
;;   new  - buffer entry describing the destination, or #false for 'delete
;;
;; A rename is just a 'move whose old and new share a parent.
(struct change (kind old new) #:transparent)

(fun entry->path :: (e entry? -> string?)
  (string-append (entry-parent e) "/" (entry-name e)))

(fun change-source :: (c change? -> any/c)
  (if (change-old c) (entry->path (change-old c)) #f))

(fun change-dest :: (c change? -> any/c)
  (if (change-new c) (entry->path (change-new c)) #f))

(fun change->string :: (c change? -> string?)
  (case (change-kind c)
    [(create) (string-append "CREATE " (change-dest c)
                (if (equal? (entry-type (change-new c)) 'directory) "/" ""))]
    [(delete) (string-append "DELETE " (change-source c))]
    [(move) (string-append "MOVE   " (change-source c) " -> " (change-dest c))]
    [(copy) (string-append "COPY   " (change-source c) " -> " (change-dest c))]
    [else (string-append "?????  " (symbol->string (change-kind c)))]))

;; `state` overlays the fs: path -> #true (will exist) / #false (vacated earlier in the batch)
(define (simulated-exists? state path)
  (if (hash-contains? state path) (hash-ref state path) (path-exists? path)))

(define (name-problem e)
  (define n (entry-name e))
  (cond
    [(equal? n "") "empty name"]
    [(string-contains? n "/") (string-append "name contains a slash: " n)]
    [(string-contains? n "\n") (string-append "name contains a newline: " n)]
    [else #f]))

(define (check-change c state)
  (define src (change-source c))
  (define dst (change-dest c))
  (define bad-name (if (change-new c) (name-problem (change-new c)) #f))
  (define parent (if (change-new c) (entry-parent (change-new c)) #f))
  (define problems
    (filter (fn (p) p)
      (list
        bad-name
        (if (and src (not (simulated-exists? state src)))
          (string-append "source is gone: " src)
          #f)
        (if (and dst (not bad-name) (simulated-exists? state dst))
          (string-append "would overwrite: " dst)
          #f)
        (if (and parent (not bad-name) (not (simulated-exists? state parent)))
          (string-append "no such directory: " parent)
          #f))))
  (define vacated
    (if (and src (member (change-kind c) '(move delete)))
      (hash-insert state src #f)
      state))
  (list problems (if dst (hash-insert vacated dst #t) vacated)))

;; empty = safe to apply in this order
(fun validate-changes :: (changes (listof change?) -> (listof string?))
  (define (loop cs state acc)
    (if (empty? cs)
      (reverse acc)
      (match (check-change (car cs) state)
        [(list problems next)
          (loop (cdr cs)
            next
            (append
              (map (fn (p) (string-append (change->string (car cs)) "  !! " p)) problems)
              acc))])))
  (loop changes (hash) '()))
