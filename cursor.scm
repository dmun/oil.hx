(require "helix/editor.scm")
(require "helix/static.scm")
(require-builtin helix/core/text)

(provide clamp-cursors!)

(define olive-min-cursor-col 5)

(define (range-cursor-pos r)
  (let ([anchor (range-anchor r)]
        [head (range-head r)])
    (if (> head anchor) (- head 1) head)))

(define (olive-range-adjust-target rope r)
  (let* ([cursor (range-cursor-pos r)]
         [line (rope-char->line rope cursor)]
         [line-start (rope-line->char rope line)]
         [boundary (+ line-start olive-min-cursor-col)]
         [line-length (rope-len-chars (rope->line rope line))])
    (and (< cursor boundary)
         (< olive-min-cursor-col line-length)
         (if (or (= cursor line-start)
                 (= line 0))
           boundary
           (- line-start 1)))))

(define (adjust-olive-range rope r)
  (let ([target (olive-range-adjust-target rope r)])
    (if target (range target target) r)))

(define (any? predicate values)
  (and (not (empty? values))
       (or (predicate (car values))
           (any? predicate (cdr values)))))

(define (primary-last ranges primary-index)
  (define (loop remaining index others primary)
    (if (empty? remaining)
      (append (reverse others) (list primary))
      (if (= index primary-index)
        (loop (cdr remaining) (+ index 1) others (car remaining))
        (loop (cdr remaining) (+ index 1) (cons (car remaining) others) primary))))
  (loop ranges 0 '() #f))

(define (set-olive-ranges! ranges primary-index)
  (let ([ordered (primary-last ranges primary-index)])
    (set-current-selection-object! (range->selection (car ordered)))
    (for-each push-range-to-selection! (cdr ordered))))

(define (clamp-cursors! view-id)
  (let* ([rope (editor->text (editor->doc-id view-id))]
         [selection (current-selection-object)]
         [ranges (selection->ranges selection)]
         [adjusted (map (fn (r) (adjust-olive-range rope r)) ranges)])
     (when (any? (fn (r) (olive-range-adjust-target rope r)) ranges)
       (set-olive-ranges! adjusted (selection->primary-index selection)))))
