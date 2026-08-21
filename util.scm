(provide fun
         fun-build
         false?
         concat
         box-update!
         match-result
         with-delay
         with-doc
         schedule
         dbg!)

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

;; append kapoet
(define (concat a b) (foldl cons b (reverse a)))

(define (box-update! b f) (set-box! b (f (unbox b))))

(define-syntax match-result
  (syntax-rules ()
    [(_ expr
        (val ok-body ...)
        (err err-body ...))
     (let ((result expr))
       (cond
         ((Ok? result)
          (let ((val (Ok->value result)))
            ok-body ...))
         ((Err? result)
          (let ((err (Err->value result)))
            err-body ...))))]))

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

(define-syntax dbg!
  (syntax-rules ()
    [(_ expr)
     (let ([v expr])
       (log::debug! (list 'expr '=> v))
       v)]))
