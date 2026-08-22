(require "helix/editor.scm")
(require "helix/static.scm")

(require "util.scm")
(require "macros.scm")

(provide *oil-target*)
(define *oil-target* (box #f))

(provide-fun position-oil-target! :: (doc-id doc-id? -> dir string? -> void?)
  (define pending (unbox *oil-target*))
  (define target (and pending (equal? (car pending) dir) (cdr pending)))
  (when (and target
             (= (doc-id->usize doc-id)
                (doc-id->usize (editor->doc-id (editor-focus)))))
    (let ([old-search (register->value #\/)]
          [pattern (string-append "(?-i)^/[0-9]+ "
                                  (escape-regex-string target)
                                  "$")])
      (set-register! #\/ (list pattern))
      (goto_file_start)
      (search_next)
      (goto_line_start)
      (set-register! #\/ old-search)
      (set-box! *oil-target* #f))))
