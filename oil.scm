(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require "helix/components.scm")
(require "helix/configuration.scm")
(require-builtin helix/core/text as text.)

(require "steel/result")

(require (prefix-in hx. "helix/commands.scm"))

(require "entry.scm")
(require "change.scm")
(require "debug.scm")
(require "util.scm")

(define *ignore-next-save* #f)
(define *oil-doc-ids* '())

(define *next-id* 1)
;; url -> (hash name -> entry)
(define *directory-cache* (box (hash)))
(define *entries-by-id* (hash))
;; old contents of a url, during a re-read
(define *directory-desired* (box (hash)))

(provide oil-open)

(define (directory-entries path)
  (define iter (read-dir-iter path))
  (define (collect entries)
    (let ([entry (read-dir-iter-next! iter)])
      (if entry
        (collect
          (cons (read-dir-entry-file-name entry)
            entries))
        (reverse entries))))
  (collect '()))

(define (set-document-lines! lines)
  (select_all)
  (replace-selection-with
    (string-join lines "\n")))

(define (read-dir-entry-file-type e)
  (cond
    [(read-dir-entry-is-symlink? e) 'link]
    [(read-dir-entry-is-dir? e) 'directory]
    [else 'file]))

(define (list-url parent)
  (or (hash-try-get (unbox *directory-cache*) parent) (hash)))

(define (directory-read? parent)
  (hash-contains? (unbox *directory-cache*) parent))

(define (entry-by-id id)
  (hash-try-get *entries-by-id* id))

(define (entry-by-name parent name)
  (hash-try-get (list-url parent) name))

;; unordered
(define (entries-in parent)
  (hash-values->list (list-url parent)))

(define (pending-entry parent name)
  (define desired (unbox *directory-desired*))
  (and (hash-contains? desired parent)
    (hash-try-get (hash-ref desired parent) name)))

(fun store-entry! :: (storage any/c -> e entry? -> void?)
  (define parent (entry-parent e))
  (define name (entry-name e))
  (define storage-value (unbox storage))
  (define entries (or (hash-try-get storage-value parent) (hash)))
  (set-box! storage
    (hash-insert storage-value parent (hash-insert entries name e)))
  void)

;; reuses the existing entry for parent/name, so ids survive a re-read
(define (create-entry! parent name type)
  (define existing (or (entry-by-name parent name)
                    (pending-entry parent name)))
  (define e (if existing
             existing
             (entry *next-id* parent name type #f)))
  (unless existing
    (set! *next-id* (+ *next-id* 1)))
  (set-entry-type! e type)
  (store-entry! *directory-cache* e)
  (set! *entries-by-id* (hash-insert *entries-by-id* (entry-id e) e))
  (when (pending-entry parent name)
    (define desired (unbox *directory-desired*))
    (set-box! *directory-desired*
      (hash-insert desired
        parent
        (hash-remove (hash-ref desired parent) name))))
  e)

(define (begin-update! parent)
  (set-box! *directory-desired*
    (hash-insert (unbox *directory-desired*) parent (list-url parent)))
  (set-box! *directory-cache*
    (hash-insert (unbox *directory-cache*) parent (hash))))

(define (end-update! parent)
  (define desired (unbox *directory-desired*))
  (when (hash-contains? desired parent)
    ;; whatever is left was deleted on disk
    (for-each
      (lambda (e)
        (set! *entries-by-id* (hash-remove *entries-by-id* (entry-id e))))
      (hash-values->list (hash-ref desired parent)))
    (set-box! *directory-desired* (hash-remove desired parent))))

(define (add-entries! path)
  (define iter (read-dir-iter path))
  (begin-update! path)
  (let loop ()
    (let ([e (read-dir-iter-next! iter)])
      (when e
        (create-entry! path
          (read-dir-entry-file-name e)
          (read-dir-entry-file-type e))
        (loop))))
  (end-update! path))

(define (oil-open)
  (define dir (current-directory))
  (define tmp-dir (string-append "/tmp/oil" dir ".d"))
  (define uri (string-append "oil://" dir))

  (hx.new)
  ; (set! *ignore-next-save* #t)
  (hx.write! tmp-dir)

  ; wait
  (enqueue-thread-local-callback
    (lambda ()
      (define doc-id (editor->doc-id (editor-focus)))
      (set! *oil-doc-ids* (cons doc-id *oil-doc-ids*))))

  (set-buffer-uri! uri)
  (add-entries! dir)

  (define (pad-id n width)
    (define s (number->string n))
    (define need (- width (string-length s)))
    (if (> need 0) (string-append (make-string need #\0) s) s))
  (set-document-lines!
    (map (lambda (entry)
          (string-append
            "/"
            (pad-id (entry-id entry) 3)
            " "
            (oil-render-name entry)))
      (entries-in dir))))

(define x (cons "/003" "bruh.txt"))

(fun oil-string-id->int :: (id string? -> (Result/c int? string?))
  (with-handler
    (fn (_)
      (Err "failed to parse oil id"))
    (Ok (string->int (list->string (string->list id 1))))))

(fun gen-id! :: (int?)
  (define id *next-id*)
  (set! *next-id* (+ *next-id* 1))
  id)

(fun oil-name-type :: (name string? -> symbol?)
  (if (ends-with? name "/") 'directory 'file))

(fun oil-bare-name :: (name string? -> string?)
  (trim-end-matches name "/"))

(fun oil-render-name :: (e entry? -> string?)
  (if (equal? (entry-type e) 'directory)
    (string-append (entry-name e) "/")
    (entry-name e)))

(fun oil-line->entry :: (dir string? -> line string? -> (Result/c entry? string?))
  (define trimmed (trim line))
  (match (split-once trimmed " ")
    [(list raw-id name)
      (map-ok
        (oil-string-id->int raw-id)
        (fn (id)
          (entry id dir (oil-bare-name name) (oil-name-type name) #f)))]
    [#t (Ok (entry (gen-id!) dir (oil-bare-name trimmed) (oil-name-type trimmed) #f))]
    [_ (Err "failed to parse line")]))

(fun parse-oil-document :: (dir string? -> text string? -> (Result/c (listof entry?) string?))
  (define (loop lines entries)
    (if (empty? lines)
      (Ok entries)
      (ok-and-then
        (oil-line->entry dir (car lines))
        (lambda (entry)
          (map-ok
            (loop (cdr lines) entries)
            (lambda (rest)
              (cons entry rest)))))))
  (loop (split-many (trim text) "\n") '()))

(fun new->changes :: (dir string? -> new (listof entry?) -> (listof change?))
  (define (loop entries seen acc)
    (if (empty? entries)
      (cons (reverse acc) seen)
      (let* ([n (car entries)]
             [id (entry-id n)]
             [o (entry-by-id id)]
             [c (cond
                 ;; unknown id: a line the user typed
                 [(not o) (change 'create #f n)]
                 [(hashset-contains? seen id) (change 'copy o n)]
                 [(and (equal? (entry-parent o) (entry-parent n))
                     (equal? (entry-name o) (entry-name n)))
                   #f]
                 [else (change 'move o n)])])
        (loop (cdr entries)
          (hashset-insert seen id)
          (if c (cons c acc) acc)))))

  (define result (loop new (hashset) '()))
  (define changes (car result))
  (define seen (cdr result))

  ;; whatever the cache has for `dir` and the buffer never mentioned is a delete
  (append changes
    (map (lambda (o) (change 'delete o #f))
      (filter (lambda (o) (not (hashset-contains? seen (entry-id o))))
        (entries-in dir)))))

; (fun validate-changes :: (changes (listof change?) -> (Result/c (listof change?) string?))
; ())

; (fun execute-changes! :: (changes (listof change?) -> (Result/c (listof change?) string?))
;      (for-each (lambda (change)
; (case (change-kind change)
;       (('create) (create-directory! (string-append (change)))))
;                        ) changes))

(define (oil-save!)
  (for-each
    (lambda (doc-id)
      (define doc-text (text.rope->string (editor->text doc-id)))
      (define new (ok-and-then
                   (parse-oil-document (current-directory) doc-text)
                   (fn (x) x)))
      (define changes (new->changes (current-directory) new))
      (define problems (validate-changes changes))
      ;; dry run: nothing touches disk until this comes back empty
      (if (empty? problems)
        (map (fn (change) (dbg! (change->string change))) changes)
        problems)
      doc-id)
    *oil-doc-ids*)
  (push-component!
    (prompt "apply changes? (y/n): "
      (lambda (input)
        (when (equal? input "y")
          (dbg! "stinkyyyyyy"))))))

(register-hook 'document-saved
  (lambda (doc-id)
    (when (member
           (doc-id->usize doc-id)
           (map doc-id->usize *oil-doc-ids*))
      (if *ignore-next-save*
        (set! *ignore-next-save* #f)
        (oil-save!)))))
