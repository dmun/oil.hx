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

(fun oil-preview-lines :: (changes (listof change?) -> (listof string?))
  (define problems (validate-changes changes))
  (if (empty? problems)
    (map change->string changes)
    problems))

(define (oil-show-preview! lines on-confirm)
  (define prompt "[Y]es  [N]o")
  (define width (foldl max 0 (map string-length (cons prompt lines))))
  (define spacing
    (make-string (quotient (- width (string-length prompt)) 2) #\space))
  (define state (append lines (list "\n" (string-append spacing prompt))))
  (define component
    (new-component! "oil-preview"
      state
      (lambda (state rect frame)
        (define inner (oil-popup-rect state rect))
        (buffer/clear-with frame inner (theme-scope-ref "ui.popup"))
        (widget/list/render frame
          (area
            (+ (area-x inner) 1)
            (+ (area-y inner) 1)
            (- (area-width inner) 2)
            (- (area-height inner) 2))
          (widget/list state)))
      (hash "handle_event"
        (lambda (state event)
          (define c (key-event-char event))
          (cond
            [(or (equal? c #\y) (equal? c #\Y)) (on-confirm) event-result/close]
            [(or (equal? c #\n) (equal? c #\N) (key-event-escape? event))
              event-result/close]
            [else event-result/consume])))))
  (push-component! component))

;; centred, just big enough for `lines`
(define (oil-popup-rect lines rect)
  (define w (+ 2 (foldl max 0 (map string-length lines))))
  (define h (+ 2 (length lines)))
  (area
    (+ (area-x rect) (quotient (- (area-width rect) w) 2))
    (+ (area-y rect) (quotient (- (area-height rect) h) 2))
    w
    h))

(define (oil-save!)
  (for-each
    (lambda (doc-id)
      (define doc-text (text.rope->string (editor->text doc-id)))
      (define new (ok-and-then
                   (parse-oil-document (current-directory) doc-text)
                   (fn (x) x)))
      (define changes (new->changes (current-directory) new))
      (if (empty? changes)
        (set-status! "oil: no changes")
        (oil-show-preview! (oil-preview-lines changes)
          (lambda () (set-status! "oil: apply not implemented"))))
      doc-id)
    *oil-doc-ids*))

(register-hook 'document-saved
  (lambda (doc-id)
    (when (member
           (doc-id->usize doc-id)
           (map doc-id->usize *oil-doc-ids*))
      (if *ignore-next-save*
        (set! *ignore-next-save* #f)
        (oil-save!)))))
