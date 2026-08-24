(provide fun
         fun/provide
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
(define-syntax fun/parse
  (syntax-rules (-> void?)
    ;; void-returning functions implicitly end in `void`
    [(fun/parse name ((Result/c void? err)) (arg ...) (contract ...) body ...)
     (define/contract (name arg ...)
       (->/c contract ... (Result/c void? err))
       body ...
       (Ok void))]

    ;; generic return contract
    [(fun/parse name (return) (arg ...) (contract ...) body ...)
     (define/contract (name arg ...)
       (->/c contract ... return)
       body ...)]

    [(fun/parse name
                (next-arg next-contract -> rest ...)
                (arg ...)
                (contract ...)
                body ...)
     (fun/parse name
                (rest ...)
                (arg ... next-arg)
                (contract ... next-contract)
                body ...)]))

(define-syntax fun
  (syntax-rules (::)
    [(fun name :: signature body ...)
     (fun/parse name signature () () body ...)]))

(define-syntax fun/provide
  (syntax-rules (::)
    [(fun/provide name :: signature body ...)
     (begin
       (fun/parse name signature () () body ...)
       (provide name))]))

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
