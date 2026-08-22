(require "helix/editor.scm")
(require "helix/static.scm")
(require-builtin helix/core/text)

(provide clamp-cursors!)

(define oil-min-cursor-col 5)

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
    (and (< cursor boundary)
         (< oil-min-cursor-col line-length)
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

(define (clamp-cursors! view-id)
  (let* ([rope (editor->text (editor->doc-id view-id))]
         [selection (current-selection-object)]
         [ranges (selection->ranges selection)]
         [adjusted (map (fn (r) (adjust-oil-range rope r)) ranges)])
     (when (any? (fn (r) (oil-range-adjust-target rope r)) ranges)
       (set-oil-ranges! adjusted (selection->primary-index selection)))))
