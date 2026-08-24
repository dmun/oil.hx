(require "macros.scm")

(provide
  doc-id?
  escape-regex
  escape-regex-string
  false?
  concat
  box-update!)

(fun/provide doc-id? :: (v any/c -> bool?)
  (with-handler (fn (_) #f) (begin (doc-id->usize v) #t)))

(fun/provide escape-regex :: (char char? -> string?)
  (let ([chars (string->list "\\.+*?()|[]{}^$#&-~")])
    (if (member char chars)
      (string #\\ char)
      (string char))))

(fun/provide escape-regex-string :: (text string? -> string?)
  (apply string-append (map escape-regex (string->list text))))

(define (false? v) (equal? v #f))

;; append kapoet
(define (concat a b) (foldl cons b (reverse a)))

(define (box-update! b f) (set-box! b (f (unbox b))))

