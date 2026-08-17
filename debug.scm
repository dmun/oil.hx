(provide dbg!)
(define-syntax dbg!
  (syntax-rules ()
    [(_ expr)
      (let ([v expr])
        (log::debug! (list 'expr '=> v))
        v)]))
