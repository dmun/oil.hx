(require "entry.scm")
(require "util.scm")

(provide cache-snapshot
         cache-entries-in
         cache-next-id!
         cache-refresh-dir!
         cache-add!
         cache-move!
         cache-remove!)

;; The canonical snapshot of filesystem entries. Entries keep stable IDs across
;; directory refreshes and moves so edited oil buffers can be diffed reliably.
(define *next-id* (box 1))
(define *entries* (box (hash)))

(define (cache-snapshot)
  (unbox *entries*))

;; unordered
(fun cache-entries-in :: (parent string? -> (listof entry?))
  (filter
    (fn (e) (equal? (entry-parent e) parent))
    (hash-values->list (cache-snapshot))))

(fun cache-next-id! :: (int?)
  (define id (unbox *next-id*))
  (set-box! *next-id* (+ id 1))
  id)

(fun cache-store-entry! :: (e entry? -> any/c)
  (box-update! *entries*
    (fn (cache) (hash-insert cache (entry-id e) e))))

;; Replaces one directory's slice of the canonical cache in one update.
(define (cache-replace-directory! parent entries)
  (define retained
    (foldl
      (fn (e cache)
        (if (equal? (entry-parent e) parent)
          cache
          (hash-insert cache (entry-id e) e)))
      (hash)
      (hash-values->list (cache-snapshot))))
  (set-box! *entries*
    (foldl
      (fn (e cache) (hash-insert cache (entry-id e) e))
      retained
      entries)))

(define (read-dir-entry-file-type e)
  (cond
    [(read-dir-entry-is-symlink? e) 'link]
    [(read-dir-entry-is-dir? e) 'directory]
    [else 'file]))

(define (cache-refresh-dir! dir)
  (define previous
    (foldl
      (fn (e by-name) (hash-insert by-name (entry-name e) e))
      (hash)
      (cache-entries-in dir)))
  (define iter (read-dir-iter dir))
  (define (loop found)
    (let ([e (read-dir-iter-next! iter)])
      (if e
        (let* ([name (read-dir-entry-file-name e)]
               [old (hash-try-get previous name)]
               [fresh (entry
                        (if old (entry-id old) (cache-next-id!))
                        dir
                        name
                        (read-dir-entry-file-type e)
                        (if old (entry-metadata old) #f))])
          (loop (cons fresh found)))
        (cache-replace-directory! dir (reverse found)))))
  (loop '()))

(define (cache-add! e)
  (cache-store-entry!
    (entry (cache-next-id!)
           (entry-parent e)
           (entry-name e)
           (entry-type e)
           #f)))

(define (cache-move! src dest)
  (cache-store-entry!
    (entry (entry-id src)
           (entry-parent dest)
           (entry-name dest)
           (entry-type src)
           (entry-metadata src))))

(define (cache-remove! e)
  (box-update! *entries*
    (fn (cache) (hash-remove cache (entry-id e)))))
