(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")

(require-builtin helix/core/text as text.)

(require "steel/result")

(require (prefix-in hx. "helix/commands.scm"))

(require "entry.scm")
(require "action.scm")
(require "ui.scm")
(require "util.scm")

;; doc-id usize -> (doc-id . directory url)
(define *oil-docs* (box (hash)))

;; doc-id usizes whose next 'document-saved is our own write, not the user's
(define *oil-ignore* (box (hash)))

(define *next-id* (box 1))
(define *entries* (box (hash)))

(provide oil-open oil-parent)

(define (set-document-lines! lines)
  (select_all)
  (replace-selection-with
    ;; trailing newline to avoid extra write if 'insert-final-newline' set
    (string-append (string-join lines "\n") "\n")))

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

;; unordered
(fun entries-in :: (cache hash? -> parent string? -> (listof entry?))
  (filter
    (fn (e) (equal? (entry-parent e) parent))
    (hash-values->list cache)))

(fun store-entry! :: (e entry? -> any/c)
  (box-update! *entries*
    (fn (cache) (hash-insert cache (entry-id e) e))))

;; Replaces one directory's slice of the canonical cache in one update.
(define (replace-directory! parent entries)
  (define retained
    (foldl
      (fn (e cache)
        (if (equal? (entry-parent e) parent)
          cache
          (hash-insert cache (entry-id e) e)))
      (hash)
      (hash-values->list (unbox *entries*))))
  (set-box! *entries*
    (foldl
      (fn (e cache) (hash-insert cache (entry-id e) e))
      retained
      entries)))

(define (add-entries! path)
  (define previous
    (foldl
      (fn (e by-name) (hash-insert by-name (entry-name e) e))
      (hash)
      (entries-in (unbox *entries*) path)))
  (define iter (read-dir-iter path))
  (define (loop found)
    (let ([e (read-dir-iter-next! iter)])
      (if e
        (let* ([name (read-dir-entry-file-name e)]
               [old (hash-try-get previous name)]
               [fresh (entry
                        (if old (entry-id old) (gen-id!))
                        path
                        name
                        (read-dir-entry-file-type e)
                        (if old (entry-metadata old) #f))])
          (loop (cons fresh found)))
        (replace-directory! path (reverse found)))))
  (loop '()))

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
    (sort (entries-in (unbox *entries*) dir) entry<?)))

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
      (ok-and-then (oil-line->entry dir (car lines))
                   (fn (e)
                       (loop (cdr lines)
                             (cons e parsed))))))
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
          (fn (entries) (loop (cdr remaining) (append (reverse entries) parsed)))))))
  (loop dirs '()))

;; #t if `cmd` exited 0; anything else is reported on the status line
(define (run! cmd args)
  (define status (unwrap-ok (wait (unwrap-ok (spawn-process (command cmd args))))))
  (if (equal? status 0)
    #t
    (begin
      (set-status! (string-append "oil: " cmd " exited " (number->string status)))
      #f)))

(define (create-path! e)
  (define (create-file! path)
    (close-output-port (open-output-file path)))
  (if (directory? e)
    (create-directory! (entry->path e))
    (close-output-port (open-output-file (entry->path e)))))

;; a symlink is unlinked like a file, whatever it points at
(define (delete-path! e)
  (if (directory? e)
    (delete-directory! (entry->path e))
    (delete-file! (entry->path e))))

(define (cache-add! e)
  (store-entry!
    (entry (gen-id!)
           (entry-parent e)
           (entry-name e)
           (entry-type e)
           #f)))

(define (cache-move! src dest)
  (store-entry!
    (entry (entry-id src)
           (entry-parent dest)
           (entry-name dest)
           (entry-type src)
           (entry-metadata src))))

(define (cache-remove! e)
  (box-update! *entries* (fn (cache) (hash-remove cache (entry-id e)))))

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
  (with-delay 50 (set-status! ""))
  (schedule (with-doc doc-id (redo))))

(fun oil-save! :: (doc-id doc-id? -> void?)
  (define docs (oil-docs-by-dir))
  (define dirs (hash-keys->list docs))
  (match-result (oil-docs->entries docs dirs)
    [entries (try-write! doc-id docs dirs
               (order-actions
                 (entries->actions (unbox *entries*) dirs entries)))]
    [err (with-delay 50 (set-error! (string-append "oil: " err)))]))

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
