(require "helix/editor.scm")
(require "helix/misc.scm")
(require "helix/static.scm")

(require-builtin helix/core/text)

(require "steel/result")

(require (prefix-in hx. "helix/commands.scm"))

(require "entry.scm")
(require "cache.scm")
(require "action.scm")
(require "ui.scm")
(require "util.scm")

;; doc-id usize -> (doc-id . directory url)
(define *oil-docs* (box (hash)))

;; doc-id usizes whose next 'document-saved is our own write, not the user's
(define *oil-ignore* (box (hash)))

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
       (sort (cache-entries-in dir) entry<?)))

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

(define (rope-char->col rope pos)
  (- pos
     (rope-line->char
       rope
       (rope-char->line rope pos))))

(define oil-min-cursor-col 5)

;; A forward range's head is after its block cursor; a backward range's head
;; is the cursor.  The protected prefix is ASCII, so stepping back one char is
;; sufficient for any cursor that can be to its left.
(define (range-cursor-pos r)
  (let ([anchor (range-anchor r)]
        [head (range-head r)])
    (if (> head anchor) (- head 1) head)))

(define (oil-range-adjust-target rope r)
  (let* ([cursor (range-cursor-pos r)]
         [line (rope-char->line rope cursor)]
         [line-start (rope-line->char rope line)]
         [boundary (+ line-start oil-min-cursor-col)]
         [line-length (rope-len-chars (rope->line rope line))])
    ;; Short lines (notably the trailing empty line) have no valid position at
    ;; the boundary.  Leave them alone until the boundary exists.
    (and (< cursor boundary)
         (< oil-min-cursor-col line-length)
         ;; Moving forward past an end-of-line lands at column zero of the
         ;; next line, so advance that cursor to its editable boundary.  Other
         ;; positions in the prefix come from backward motions such as `h` or
         ;; `b` and wrap to the preceding line.  The first line cannot wrap.
         (if (or (= cursor line-start)
                 (= line 0))
           boundary
           (- line-start 1)))))

(define (adjust-oil-range rope r)
  (let ([target (oil-range-adjust-target rope r)])
    (if target (range target target) r)))

(define (any? predicate values)
  (and (not (empty? values))
       (or (predicate (car values))
           (any? predicate (cdr values)))))

;; The mutating selection API makes every pushed range primary.  Add the
;; original primary last so it remains primary after Helix normalizes ranges.
(define (primary-last ranges primary-index)
  (define (loop remaining index others primary)
    (if (empty? remaining)
      (append (reverse others) (list primary))
      (if (= index primary-index)
        (loop (cdr remaining) (+ index 1) others (car remaining))
        (loop (cdr remaining) (+ index 1) (cons (car remaining) others) primary))))
  (loop ranges 0 '() #f))

(define (set-oil-ranges! ranges primary-index)
  (let ([ordered (primary-last ranges primary-index)])
    (set-current-selection-object! (range->selection (car ordered)))
    (for-each push-range-to-selection! (cdr ordered))))

(register-hook 'selection-did-change
               (fn (view-id)
                   ;; The selection mutation API operates on the focused view, while this
                   ;; hook can also report changes from another view of the same document.
                   (when (and (equal? view-id (editor-focus))
                              (oil-doc? (editor->doc-id view-id)))
                     (let* ([rope (editor->text (editor->doc-id view-id))]
                            [selection (current-selection-object)]
                            [ranges (selection->ranges selection)]
                            [adjusted (map (fn (r) (adjust-oil-range rope r)) ranges)])
                       ;; Avoid recursively firing the hook when every cursor is already in
                       ;; bounds.  Each violating multicursor is transformed independently.
                       (when (any? (fn (r) (oil-range-adjust-target rope r)) ranges)
                         (set-oil-ranges! adjusted (selection->primary-index selection)))))))

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
