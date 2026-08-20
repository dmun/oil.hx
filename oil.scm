(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require "helix/components.scm")
(require-builtin helix/core/text as text.)

(require "steel/result")

(require (prefix-in hx. "helix/commands.scm"))

(require "entry.scm")
(require "change.scm")
(require "debug.scm")
(require "util.scm")

;; doc-id usize -> (doc-id . directory url)
(define *oil-docs* (hash))
;; doc-id usizes whose next 'document-saved is our own write, not the user's
(define *oil-ignore* (hash))

(define *next-id* 1)
;; url -> (hash name -> entry)
(define *directory-cache* (box (hash)))
(define *entries-by-id* (hash))
;; old contents of a url, during a re-read
(define *directory-desired* (box (hash)))

(provide oil-open oil-parent)

(define (clear-status!) (set-status! ""))

(define (set-document-lines! lines)
  (select_all)
  (replace-selection-with
    ;; trailing newline to avoid extra write if 'insert-final-newline' set
    (string-append (string-join lines "\n") "\n")))

;; run `thunk` with `doc-id` shown in the focused view, switching back after
(define (with-doc! doc-id thunk)
  (define prev (editor->doc-id (editor-focus)))
  (if (equal? (doc-id->usize prev) (doc-id->usize doc-id))
    (thunk)
    (begin
      (editor-switch-action! doc-id (Action/Replace))
      (thunk)
      (editor-switch-action! prev (Action/Replace)))))

(define (ignore-next-save! doc-id)
  (set! *oil-ignore* (hash-insert *oil-ignore* (doc-id->usize doc-id) #t)))

;; render `dir`'s listing into `doc-id` and persist it
(define (oil-render! doc-id dir)
  (when (editor-doc-exists? doc-id)
    (with-doc! doc-id
      (lambda ()
        (set-document-lines! (oil-listing-lines dir))
        (ignore-next-save! doc-id)
        (hx.write!)))))

(define (read-dir-entry-file-type e)
  (cond
    [(read-dir-entry-is-symlink? e) 'link]
    [(read-dir-entry-is-dir? e) 'directory]
    [else 'file]))

(define (list-url parent)
  (or (hash-try-get (unbox *directory-cache*) parent) (hash)))

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

(fun store-entry! :: (e entry? -> void?)
  (define parent (entry-parent e))
  (define cache (unbox *directory-cache*))
  (set-box! *directory-cache*
    (hash-insert cache parent (hash-insert (list-url parent) (entry-name e) e)))
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
  (store-entry! e)
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
  (hx.open (oil-tmp-path (current-directory))))

;; from an oil buffer: go up; from a file: open its directory
(define (oil-parent)
  (define path (editor-document->path (editor->doc-id (editor-focus))))
  (define dir (or (oil-tmp->dir path) path (current-directory)))
  (hx.open (oil-tmp-path (parent-name dir))))

;; "/tmp/oil<dir>.d" -> dir, or #f if `path` is not an oil tmp path
(define (oil-tmp->dir path)
  (and path
    (starts-with? path "/tmp/oil")
    (ends-with? path ".d")
    (trim-end-matches (trim-start-matches path "/tmp/oil") ".d")))

;; any document opened under the oil tmp prefix becomes an oil buffer
(register-hook 'document-opened
  (lambda (doc-id)
    (define dir (oil-tmp->dir (editor-document->path doc-id)))
    (when dir
      (set! *oil-docs*
        (hash-insert *oil-docs* (doc-id->usize doc-id) (cons doc-id dir)))
      (enqueue-thread-local-callback
        (lambda ()
          (when (editor-doc-exists? doc-id)
            (add-entries! dir)
            (with-doc! doc-id
              (lambda () (set-buffer-uri! (string-append "oil://" dir))))
            ;; also persists, so tmp parent dirs exist and a plain :w works
            (oil-render! doc-id dir)))))))

(register-hook 'document-closed
  (lambda (e)
    (define id (doc-id->usize (doc-closed-id e)))
    (when (hash-contains? *oil-docs* id)
      (set! *oil-docs* (hash-remove *oil-docs* id)))
    (when (hash-contains? *oil-ignore* id)
      (set! *oil-ignore* (hash-remove *oil-ignore* id)))))

(define (oil-listing-lines dir)
  (define (pad-id n width)
    (define s (number->string n))
    (define need (- width (string-length s)))
    (if (> need 0) (string-append (make-string need #\0) s) s))
  (map (lambda (entry)
        (string-append "/" (pad-id (entry-id entry) 3) " " (oil-render-name entry)))
    (sort (entries-in dir) entry<?)))

;; directories first, then alphabetic
(define (entry<? a b)
  (define dir-a (equal? (entry-type a) 'directory))
  (define dir-b (equal? (entry-type b) 'directory))
  (if (equal? dir-a dir-b)
    (string<? (entry-name a) (entry-name b))
    dir-a))

(define (oil-tmp-path dir) (string-append "/tmp/oil" dir ".d"))

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

(fun new->changes :: (dirs (listof string?) -> new (listof entry?) -> (listof change?))
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

  ;; whatever the cache lists for `dirs` and no buffer mentioned is a delete.
  ;; built with cons folds: steel 0.8.2's `append` corrupts 5-8 element
  ;; lists coming out of map/filter chains (length ok, iterates as empty)
  (define deletes
    (foldl
      (lambda (dir acc)
        (foldl
          (lambda (o acc)
            (if (hashset-contains? seen (entry-id o))
              acc
              (cons (change 'delete o #f) acc)))
          acc
          (entries-in dir)))
      '()
      dirs))
  (append changes deletes))

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

(define (create-file! path)
  (close-output-port (open-output-file path)))

(define (run! cmd args)
  (define status (unwrap-ok (wait (unwrap-ok (spawn-process (command cmd args))))))
  (unless (equal? status 0)
    (set-status! (string-append "oil: " cmd " exited " (number->string status))))
  status)

;; drop `e` from its parent's listing (entries-by-id untouched)
(define (cache-unlist! e)
  (define parent (entry-parent e))
  (set-box! *directory-cache*
    (hash-insert (unbox *directory-cache*) parent
      (hash-remove (list-url parent) (entry-name e)))))

;; applies each change to disk and mirrors it in the cache, so ids
;; survive moves and no re-read is needed afterwards
(define (changes-apply! changes)
  (define (file? e) (eq? (entry-type e) 'file))
  (for-each
    (lambda (c)
      (define old (change-old c))
      (define new (change-new c))
      (case (change-kind c)
        [(create)
         ((if (file? new) create-file! create-directory!) (entry->path new))
         (create-entry! (entry-parent new) (entry-name new) (entry-type new))]
        [(move)
         (rename-file-or-directory! (entry->path old) (entry->path new))
         (cache-unlist! old)
         (set-entry-parent! old (entry-parent new))
         (set-entry-name! old (entry-name new))
         (store-entry! old)]
        [(delete)
         ((if (file? old) delete-file! delete-directory!) (entry->path old))
         (cache-unlist! old)
         (set! *entries-by-id* (hash-remove *entries-by-id* (entry-id old)))]
        [(copy)
         (run! "cp" (list "-a" (entry->path old) (entry->path new)))
         (create-entry! (entry-parent new) (entry-name new) (entry-type new))]
        [else (dbg! c)]))
    changes))

;; dir -> doc-id, one buffer per dir (a second buffer of a dir would double-parse)
(define (oil-dir-docs)
  (foldl (lambda (p acc) (hash-insert acc (cdr p) (car p)))
    (hash)
    (hash-values->list *oil-docs*)))

;; a save diffs *all* oil buffers at once, so cross-buffer cut/paste is a move
(define (oil-save! doc-id)
  (define docs (dbg! (oil-dir-docs)))
  (define dirs (hash-keys->list docs))
  (define entries
    (apply append
      (map
        (lambda (dir)
          (unwrap-ok
            (parse-oil-document dir
              (text.rope->string (editor->text (hash-ref docs dir))))))
        dirs)))
  (define changes (new->changes dirs entries))
  (if (empty? changes)
    (enqueue-thread-local-callback-with-delay 50
      (fn () (set-status! "oil: no changes")))
    (begin
      ;; hack: can't shadow write nicely
      (undo)
      (ignore-next-save! doc-id)
      (hx.write!)
      (enqueue-thread-local-callback-with-delay 50
        clear-status!)
      (enqueue-thread-local-callback redo)
      (oil-show-preview! (oil-preview-lines changes)
        (lambda ()
          (changes-apply! changes)
          ;; cache is already up to date; re-render every oil buffer
          (enqueue-thread-local-callback
            (lambda ()
              (for-each
                (lambda (dir) (oil-render! (hash-ref docs dir) dir))
                dirs))))))))

(register-hook 'document-saved
  (lambda (doc-id)
    (define id (doc-id->usize doc-id))
    (when (hash-contains? *oil-docs* id)
      (if (hash-contains? *oil-ignore* id)
        (set! *oil-ignore* (hash-remove *oil-ignore* id))
        (oil-save! doc-id)))))
