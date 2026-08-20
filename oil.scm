(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")
(require "helix/components.scm")
(require-builtin helix/core/text as text.)

(require "steel/result")

(require (prefix-in hx. "helix/commands.scm"))

(require "entry.scm")
(require "action.scm")
(require "debug.scm")
(require "util.scm")

;; doc-id usize -> (doc-id . directory url)
(define *oil-docs* (box (hash)))
;; doc-id usizes whose next 'document-saved is our own write, not the user's
(define *oil-ignore* (box (hash)))

(define *next-id* (box 1))
;; url -> (hash name -> entry)
(define *directory-cache* (box (hash)))
;; entry id -> entry
(define *entries-by-id* (box (hash)))
;; old contents of a url, during a re-read
(define *directory-desired* (box (hash)))
;; #t while the preview popup is up, so a second save can't diff a buffer
;; that is half-way through being restored
(define *preview-open* (box #f))

(provide oil-open oil-parent)

(define (clear-status!) (set-status! ""))

(define-syntax with-delay
  (syntax-rules ()
                ((_ delay body ...)
                 (enqueue-thread-local-callback-with-delay
                  delay
                  (fn () body ...)))))

(define-syntax schedule
  (syntax-rules ()
                ((_ body ...)
                 (enqueue-thread-local-callback
                  (fn () body ...)))))

;; run command that rely on focused doc
(define-syntax with-doc
  (syntax-rules ()
                ((_ doc-id body ...)
                 (let ([prev (editor->doc-id (editor-focus))])
                   (if (equal? (doc-id->usize prev) (doc-id->usize doc-id))
                     ((fn () body ...))
                     (begin
                       (editor-switch-action! doc-id (Action/Replace))
                       ((fn () body ...))
                       (editor-switch-action! prev (Action/Replace))))))))

(define (set-document-lines! lines)
  (select_all)
  (replace-selection-with
    ;; trailing newline to avoid extra write if 'insert-final-newline' set
    (string-append (string-join lines "\n") "\n")))

;; helix registers no predicate for DocumentId, so duck-type it: doc-id->usize
;; raises on anything else
(define (doc-id? v)
  (with-handler (fn (_) #f) (begin (doc-id->usize v) #t)))

(define (ignore-next-save! doc-id)
  (box-update! *oil-ignore*
    (fn (ignored) (hash-insert ignored (doc-id->usize doc-id) #t))))

;; render `dir`'s listing into `doc-id` and persist it
(define (oil-render! doc-id dir)
  (when (editor-doc-exists? doc-id)
    (with-doc doc-id
      (set-document-lines! (oil-listing-lines dir))
      (ignore-next-save! doc-id)
      (hx.write!))))

(define (read-dir-entry-file-type e)
  (cond
    [(read-dir-entry-is-symlink? e) 'link]
    [(read-dir-entry-is-dir? e) 'directory]
    [else 'file]))

(define (list-url parent)
  (or (hash-try-get (unbox *directory-cache*) parent) (hash)))

(define (entry-by-id id)
  (hash-try-get (unbox *entries-by-id*) id))

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
  (box-update! *directory-cache*
    (fn (cache)
      (hash-insert cache parent (hash-insert (list-url parent) (entry-name e) e))))
  void)

;; reuses the existing entry for parent/name, so ids survive a re-read
(define (create-entry! parent name type)
  (define existing (or (entry-by-name parent name) (pending-entry parent name)))
  (define e (or existing (entry (gen-id!) parent name type #f)))
  (set-entry-type! e type)
  (store-entry! e)
  (box-update! *entries-by-id* (fn (all) (hash-insert all (entry-id e) e)))
  (when (pending-entry parent name)
    (box-update! *directory-desired*
      (fn (desired)
        (hash-insert desired parent (hash-remove (hash-ref desired parent) name)))))
  e)

(define (forget-entry! e)
  (box-update! *entries-by-id* (fn (all) (hash-remove all (entry-id e)))))

(define (begin-update! parent)
  (box-update! *directory-desired*
    (fn (desired) (hash-insert desired parent (list-url parent))))
  (box-update! *directory-cache*
    (fn (cache) (hash-insert cache parent (hash)))))

(define (end-update! parent)
  (define desired (unbox *directory-desired*))
  (when (hash-contains? desired parent)
    ;; whatever is left was deleted on disk
    (for-each forget-entry! (hash-values->list (hash-ref desired parent)))
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

(define oil-tmp-prefix "/tmp/oil")
(define oil-tmp-suffix ".d")

(define (oil-tmp-path dir)
  (string-append oil-tmp-prefix dir oil-tmp-suffix))

;; "/tmp/oil<dir>.d" -> dir, or #f if `path` is not an oil tmp path.
;; Strips exactly one prefix and one suffix, so a directory of its own named
;; "foo.d" still maps back to itself.
(define (oil-tmp->dir path)
  (and path
    (starts-with? path oil-tmp-prefix)
    (ends-with? path oil-tmp-suffix)
    (substring path
      (string-length oil-tmp-prefix)
      (- (string-length path) (string-length oil-tmp-suffix)))))

;; any document opened under the oil tmp prefix becomes an oil buffer
(define (oil-listing-lines dir)
  (define (pad-id n width)
    (define s (number->string n))
    (define need (- width (string-length s)))
    (if (> need 0) (string-append (make-string need #\0) s) s))
  (map (fn (entry)
        (string-append "/" (pad-id (entry-id entry) 3) " " (oil-render-name entry)))
    (sort (entries-in dir) entry<?)))

;; directories first, then alphabetic
(define (entry<? a b)
  (define dir-a (directory? a))
  (define dir-b (directory? b))
  (if (equal? dir-a dir-b)
    (string<? (entry-name a) (entry-name b))
    dir-a))

(fun directory? :: (e entry? -> boolean?)
  (equal? (entry-type e) 'directory))

(fun oil-string-id->int :: (id string? -> (Result/c int? string?))
  (with-handler
    (fn (_)
      (Err "failed to parse oil id"))
    (Ok (string->int (list->string (string->list id 1))))))

(fun gen-id! :: (int?)
  (define id (unbox *next-id*))
  (set-box! *next-id* (+ id 1))
  id)

(fun oil-name-type :: (name string? -> symbol?)
  (if (ends-with? name "/") 'directory 'file))

(fun oil-bare-name :: (name string? -> string?)
  (trim-end-matches name "/"))

(fun oil-render-name :: (e entry? -> string?)
  (if (directory? e)
    (string-append (entry-name e) "/")
    (entry-name e)))

(fun oil-new-entry :: (dir string? -> name string? -> entry?)
  (entry (gen-id!) dir (oil-bare-name name) (oil-name-type name) #f))

;; "/007 name" is the entry with that id; anything else is a name the user
;; typed, spaces and all
(fun oil-line->entry :: (dir string? -> line string? -> (Result/c entry? string?))
  (define trimmed (trim line))
  (match (split-once trimmed " ")
    [(list raw-id name)
     (if (starts-with? raw-id "/")
       (map-ok
         (oil-string-id->int raw-id)
         (fn (id) (entry id dir (oil-bare-name name) (oil-name-type name) #f)))
       (Ok (oil-new-entry dir trimmed)))]
    [#t (Ok (oil-new-entry dir trimmed))]
    [_ (Err "failed to parse line")]))

(fun text->lines :: (text string? -> (listof string?))
  (filter
    (fn (line) (not (equal? (trim line) "")))
    (split-many text "\n")))

(fun oil-doc->entries :: (dir string? -> text string? -> (Result/c (listof entry?) string?))
  (define (loop lines parsed)
    (if (empty? lines)
      (Ok (reverse parsed))
      (let ([e (oil-line->entry dir (car lines))])
        (if (Ok? e)
          (loop (cdr lines) (cons (unwrap-ok e) parsed))
          e))))
  (loop (text->lines text) '()))

;; the entries of every oil buffer, in `dirs` order; Err from the first buffer
;; that doesn't parse
(fun oil-docs->entries :: (docs hash? -> dirs (listof string?) -> (Result/c (listof entry?) string?))
  (define (buffer-text doc-id)
    (text.rope->string (editor->text doc-id)))
  (define (loop remaining parsed)
    (if (empty? remaining)
      (Ok parsed)
      (let ([dir (car remaining)])
        (ok-and-then (oil-doc->entries dir (buffer-text (hash-ref docs dir)))
          (fn (entries) (loop (cdr remaining) (concat parsed entries)))))))
  (loop dirs '()))

;; diffs the buffer entries against the cache. The result is in buffer order,
;; which is not a safe order to apply: `order-actions` sorts that out.
(fun diff-entries :: (dirs (listof string?) -> dest (listof entry?) -> (listof action?))
  (define (loop entries seen acc)
    (if (empty? entries)
      (values (reverse acc) seen)
      (let* ([n (car entries)]
             [id (entry-id n)]
             [src (entry-by-id id)]
             [c (cond
                 [(not src) (action 'create #f n)]
                 [(hashset-contains? seen id) (action 'copy src n)]
                 [(and (equal? (entry-parent src) (entry-parent n))
                       (equal? (entry-name src) (entry-name n)))
                  #f]
                 [else (action 'move src n)])])
        (loop (cdr entries)
          (hashset-insert seen id)
          (if c (cons c acc) acc)))))

  (define-values (actions seen) (loop dest (hashset) '()))

  (define deletes
    (transduce dirs
      (compose
        (flat-mapping entries-in)
        (filtering (fn (src) (not (hashset-contains? seen (entry-id src)))))
        (mapping (fn (src) (action 'delete src #f))))
      (into-list)))
  
  (concat actions deletes))

(define (confirm! lines on-confirm)
  (define prompt "[Y]es  [N]o")
  (define width (foldl max 0 (map string-length (cons prompt lines))))
  (define spacing
    (make-string (quotient (- width (string-length prompt)) 2) #\space))
  (define state (concat lines (list "\n" (string-append spacing prompt))))
  (define (close!)
    (set-box! *preview-open* #f)
    event-result/close)
  (define component
    (new-component! "oil-preview"
      state
      (fn (state rect frame)
        (define inner (popup-area state rect))
        (buffer/clear-with frame inner (theme-scope-ref "ui.popup"))
        (widget/list/render frame
          (area
            (+ (area-x inner) 1)
            (+ (area-y inner) 1)
            (- (area-width inner) 2)
            (- (area-height inner) 2))
          (widget/list state)))
      (hash "handle_event"
        (fn (state event)
          (define c (key-event-char event))
          (cond
            [(and on-confirm (or (equal? c #\y) (equal? c #\Y)))
             (on-confirm)
             (close!)]
            [(or (equal? c #\n) (equal? c #\N) (key-event-escape? event))
             (close!)]
            [else event-result/consume])))))
  (set-box! *preview-open* #t)
  (push-component! component))

(define (popup-area lines rect)
  (let ([w (min (+ 2 (foldl max 0 (map string-length lines)))
                (area-width rect))]
        [h (min (+ 2 (length lines))
                (area-height rect))])
    (area
      (+ (area-x rect) (quotient (- (area-width rect) w) 2))
      (+ (area-y rect) (quotient (- (area-height rect) h) 2))
      w
      h)))

;; #t if `cmd` exited 0; anything else is reported on the status line
(define (run! cmd args)
  (define status (unwrap-ok (wait (unwrap-ok (spawn-process (command cmd args))))))
  (if (equal? status 0)
    #t
    (begin
      (set-status! (string-append "oil: " cmd " exited " (number->string status)))
      #f)))

;; drop `e` from its parent's listing (entries-by-id untouched)
(define (cache-unlist! e)
  (define parent (entry-parent e))
  (box-update! *directory-cache*
    (fn (cache)
      (hash-insert cache parent (hash-remove (list-url parent) (entry-name e))))))

(define (create-path! e)
  (define (create-file! path)
    (close-output-port (open-output-file path)))
  (if (directory? e)
    (create-directory! (entry->path e))
    (create-file! (entry->path e))))

;; a symlink is unlinked like a file, whatever it points at
(define (delete-path! e)
  (if (directory? e)
    (delete-directory! (entry->path e))
    (delete-file! (entry->path e))))

(define (cache-add! e)
  (create-entry! (entry-parent e) (entry-name e) (entry-type e)))

(define (cache-move! src dest)
  (cache-unlist! src)
  (set-entry-parent! src (entry-parent dest))
  (set-entry-name! src (entry-name dest))
  (store-entry! src))

(define (cache-remove! e)
  (cache-unlist! e)
  (forget-entry! e))

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
         (run! "cp" (list "-a" (entry->path src) (entry->path dest)))
         (cache-add! dest)]
        [else (error "oil: unknown action " (symbol->string (action-kind c)))])
     (Ok void))))

;; stops at the first failure, so the cache never describes a filesystem that
;; isn't there TODO
(fun actions-apply! :: (actions (listof action?) -> (Result/c void? string?))
  (define (loop remaining)
    (if (empty? remaining)
      (Ok void)
      (ok-and-then (action-apply! (car remaining))
        (fn (_) (loop (cdr remaining))))))
  (loop actions))

(define (oil-docs-by-dir)
  (foldl (fn (p acc) (hash-insert acc (cdr p) (car p)))
    (hash)
    (hash-values->list (unbox *oil-docs*))))

;; HACK: can't intercept write so reset doc
(define (restore-doc! doc-id)
  (with-doc doc-id
    (undo)
    (ignore-next-save! doc-id)
    (hx.write!))
  (with-delay 50 (clear-status!))
  (schedule (with-doc doc-id (redo))))

(fun oil-save! :: (doc-id doc-id? -> void?)
  (define docs
    (foldl
      (fn (p acc)
          (hash-insert acc (cdr p) (car p)))
      (hash)
      (hash-values->list (unbox *oil-docs*))))
  (define dirs (hash-keys->list docs))
  (define entries (oil-docs->entries docs dirs))
  (match-result entries
    [(Ok entries) (try-write! doc-id docs dirs
                   (order-actions
                     (diff-entries dirs entries)))]
    [(Err err) (with-delay 50 (set-error! (string-append "oil: " err)))]))

(define (try-write! doc-id docs dirs actions)
  (define problems (validate-actions actions))
  (restore-doc! doc-id)
  (if (empty? problems)
    (confirm! (map action->string actions)
      (fn ()
        (actions-apply! actions)
        (schedule
          (for-each (fn (dir) (oil-render! (hash-ref docs dir) dir)) dirs))))
    (with-delay 50 (set-error! (car problems)))))

(register-hook 'document-opened
  (fn (doc-id)
    (define dir (oil-tmp->dir (editor-document->path doc-id)))
    (when dir
      (box-update! *oil-docs*
        (fn (docs) (hash-insert docs (doc-id->usize doc-id) (cons doc-id dir))))
      (schedule
        (when (editor-doc-exists? doc-id)
          (add-entries! dir)
          (with-doc doc-id
            (set-buffer-uri! (string-append "oil://" dir)))
          ;; also persists, so tmp parent dirs exist and a plain :w works
          (oil-render! doc-id dir))))))

(register-hook 'document-closed
  (fn (e)
    (define id (doc-id->usize (doc-closed-id e)))
    (box-update! *oil-docs* (fn (docs) (hash-remove docs id)))
    (box-update! *oil-ignore* (fn (ignored) (hash-remove ignored id)))))

(register-hook 'document-saved
  (fn (doc-id)
    (define id (doc-id->usize doc-id))
    (when (hash-contains? (unbox *oil-docs*) id)
      (cond
        [(hash-contains? (unbox *oil-ignore*) id)
         (box-update! *oil-ignore* (fn (ignored) (hash-remove ignored id)))]
        ;; `:wa` over several oil buffers: the first save diffed them all
        [(unbox *preview-open*) void]
        [else (oil-save! doc-id)]))))
