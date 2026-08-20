(provide fun
         fun-build
         false?
         concat
         box-update!
         match-result)

;; Haskell-ish notation for define/contract:
;;
;;   (fun add-two :: (left int? -> right int? -> int?)
;;     (+ left right))
;;
;; The last element of the signature is the return contract; everything before
;; it is one `name contract ->` per argument.

;; peels the signature one argument at a time, collecting names and contracts
(define-syntax fun-build
  (syntax-rules (->)
    ;; only the return contract is left
    [(fun-build name (return) (arg ...) (contract ...) body ...)
     (define/contract (name arg ...) (->/c contract ... return) body ...)]
    [(fun-build name (next-arg next-contract -> rest ...) (arg ...) (contract ...) body ...)
     (fun-build name
                (rest ...)
                (arg ... next-arg)
                (contract ... next-contract)
                body ...)]))

(define-syntax fun
  (syntax-rules (:: ->)
    ;; no arguments, written either (int?) or (-> int?)
    [(fun name :: (-> return) body ...) (fun-build name (return) () () body ...)]
    [(fun name :: signature body ...) (fun-build name signature () () body ...)]))

(define (false? v) (equal? v #f))

;; `(append a b)`, built from cons: steel 0.8.2's `append` corrupts 5-8 element
;; lists coming out of map/filter chains (length ok, iterates as empty)
(define (concat a b) (foldl cons b (reverse a)))

(define (box-update! b f) (set-box! b (f (unbox b))))

(define-syntax match-result
  (syntax-rules (Ok Err)
    ((_ expr
        ((Ok val) ok-body ...)
        ((Err err) err-body ...))
     (let ((result expr))
       (cond
         ((Ok? result)
          (let ((val (Ok->value result)))
            ok-body ...))
         ((Err? result)
          (let ((err (Err->value result)))
            err-body ...)))))))

