(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")

(require-builtin helix/core/text)

(require "steel/result")

(require (prefix-in hx. "helix/commands.scm"))

(require "document.scm")
(require "util.scm")
(require "macros.scm")

(require "entry.scm")
(require "cache.scm")
(require "action.scm")
(require "ui.scm")
(require "cursor.scm")

;; doc-id usize -> (doc-id . directory url)
(define *oil-docs* (box (hash)))

;; doc-id usizes whose next 'document-saved is our own write, not the user's
(define *oil-ignore* (box (hash)))

(define (set-document-lines! lines)
  (select_all)
  (replace-selection-with
    ;; trailing newline to avoid extra write if 'insert-final-newline' set
    (string-append (string-join lines "\n") "\n")))

(define (ignore-next-save! doc-id)
  (box-update! *oil-ignore*
               (fn (ignored) (hash-insert ignored (doc-id->usize doc-id) #t))))

;; render `dir`'s listing into `doc-id` and persist it
(define (oil-render! doc-id dir)
  (when (editor-doc-exists? doc-id)
    (with-doc doc-id
              (set-document-lines! (oil-listing-lines dir))
              (ignore-next-save! doc-id)
              (hx.write!))
    (with-delay 50 (set-status! ""))
    (position-oil-target! doc-id dir)))

(fun olive-open :: (void?)
  (define source-path
    (editor-document->path (editor->doc-id (editor-focus))))
  (define dir (current-directory))
  (define target (and source-path (file-name source-path)))
  (define existing-doc (hash-try-get (oil-docs-by-dir) dir))
  (set-box! *oil-target* (and target (cons dir target)))
  (hx.open (oil-tmp-path dir))
  (when existing-doc
    (schedule
      (position-oil-target! existing-doc dir))))

;; from an oil buffer: go up; from a file: open its directory
(fun olive-parent :: (void?)
  (define path (editor-document->path (editor->doc-id (editor-focus))))
  (define dir (or (oil-tmp->dir path) path (current-directory)))
  (hx.open (oil-tmp-path (parent-name dir))))

(define oil-tmp-prefix "/tmp/oil")
(define oil-tmp-suffix ".oil")

(define (oil-tmp-path dir)
  (string-append oil-tmp-prefix dir oil-tmp-suffix))

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
       (sort (cache-entries-in dir) entry<?)))

;; directories first, then alphabetic
(define (entry<? a b)
  (define dir-a (directory? a))
  (define dir-b (directory? b))
  (if (equal? dir-a dir-b)
    (string<? (entry-name a) (entry-name b))
    dir-a))

(fun oil-string-id->int :: (id string? -> (Result/c int? string?))
     (with-handler
       (fn (_)
           (Err "failed to parse oil id"))
       (Ok (string->int (list->string (string->list id 1))))))

(fun oil-name-type :: (name string? -> symbol?)
     (if (ends-with? name "/") 'directory 'file))

(fun oil-bare-name :: (name string? -> string?)
     (trim-end-matches name "/"))

(fun oil-render-name :: (e entry? -> string?)
     (if (directory? e)
       (string-append (entry-name e) "/")
       (entry-name e)))

(fun oil-new-entry :: (dir string? -> name string? -> entry?)
     (entry (cache-next-id!) dir (oil-bare-name name) (oil-name-type name) #f))

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
         (ok-and-then (oil-line->entry dir (car lines))
                      (fn (e)
                          (loop (cdr lines)
                                (cons e parsed))))))
     (loop (text->lines text) '()))

;; the entries of every oil buffer, in `dirs` order; Err from the first buffer
;; that doesn't parse
(fun oil-docs->entries :: (docs hash? -> dirs (listof string?) -> (Result/c (listof entry?) string?))
     (define (buffer-text doc-id)
       (rope->string (editor->text doc-id)))
     (define (loop remaining parsed)
       (if (empty? remaining)
         (Ok (reverse parsed))
         (let ([dir (car remaining)])
           (ok-and-then (oil-doc->entries dir (buffer-text (hash-ref docs dir)))
                        (fn (entries) (loop (cdr remaining) (append (reverse entries) parsed)))))))
     (loop dirs '()))

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
  (with-delay 50 (set-status! ""))
  (schedule (with-doc doc-id (redo))))

(fun oil-save! :: (doc-id doc-id? -> void?)
     (define docs (oil-docs-by-dir))
     (define dirs (hash-keys->list docs))
     (match-result (oil-docs->entries docs dirs)
       [entries (try-write! doc-id docs dirs
                            (order-actions
                              (entries->actions (cache-snapshot) dirs entries)))]
       [err (with-delay 50 (set-error! (string-append "oil: " err)))]))

(define (try-write! doc-id docs dirs actions)
  (let ([problems (validate-actions actions)])
    (cond
      [(empty? actions)
       (with-delay 50 (set-status! "oil: no changes"))]
      [(empty? problems)
       (restore-doc! doc-id)
       (confirm! (map action->string actions)
             (fn ()
                 (actions-apply! actions)
                 (schedule
                   (for-each (fn (dir) (oil-render! (hash-ref docs dir) dir)) dirs))))]
      [else (with-delay 50 (set-error! (car problems)))])))

(register-hook 'document-opened
               (fn (doc-id)
                   (define dir (oil-tmp->dir (editor-document->path doc-id)))
                   (when dir
                     (box-update! *oil-docs*
                                  (fn (docs) (hash-insert docs (doc-id->usize doc-id) (cons doc-id dir))))
                     (schedule
                       (when (editor-doc-exists? doc-id)
                         (cache-refresh-dir! dir)
                         (with-doc doc-id
                                   (set-buffer-uri! (string-append "oil://" dir)))
                         ;; also persists, so tmp parent dirs exist and a plain :w works
                         (oil-render! doc-id dir))))))

(register-hook 'document-closed
               (fn (e)
                   (define id (doc-id->usize (doc-closed-id e)))
                   (box-update! *oil-docs* (fn (docs) (hash-remove docs id)))
                   (box-update! *oil-ignore* (fn (ignored) (hash-remove ignored id)))))

(define (oil-doc? doc-id)
  (hash-contains? (unbox *oil-docs*)
                  (doc-id->usize doc-id)))

(register-hook 'selection-did-change
               (fn (view-id)
                   (when (and (equal? view-id (editor-focus))
                              (oil-doc? (editor->doc-id view-id)))
                     (clamp-cursors! view-id))))

(register-hook 'document-saved
               (fn (doc-id)
                   (define id (doc-id->usize doc-id))
                   (when (oil-doc? doc-id)
                     (cond
                       [(hash-contains? (unbox *oil-ignore*) id)
                        (box-update! *oil-ignore* (fn (ignored) (hash-remove ignored id)))]
                       ;; `:wa` over several oil buffers: the first save diffed them all
                       [(unbox *preview-open*) void]
                       [else (oil-save! doc-id)]))))
