(require "helix/misc.scm")
(require "helix/components.scm")

(require "macros.scm")
(require "util.scm")

(provide confirm!
         *preview-open*)

(define *preview-open* (box #f))

(define (popup-area lines rect)
  (let ([w (min (+ 2 (foldl max 0 (map string-length lines)))
                (area-width rect))]
        [h (min (+ 2 (length lines))
                (area-height rect))])
    (area
      (+ (area-x rect) (quotient (- (area-width rect) w) 2))
      (+ (area-y rect) (quotient (- (area-height rect) h) 2))
      w
      h)))

(define (confirm! lines on-confirm)
  (define prompt "[Y]es  [N]o")
  (define width (foldl max 0 (map string-length (cons prompt lines))))
  (define spacing
    (make-string (quotient (- width (string-length prompt)) 2) #\space))
  (define state (concat lines (list "\n" (string-append spacing prompt))))
  (define (close!)
    (set-box! *preview-open* #f)
    event-result/close)
  (define component
    (new-component! "oil-preview"
      state
      (fn (state rect frame)
        (define inner (popup-area state rect))
        (buffer/clear-with frame inner (theme-scope-ref "ui.popup"))
        (widget/list/render frame
          (area
            (+ (area-x inner) 1)
            (+ (area-y inner) 1)
            (- (area-width inner) 2)
            (- (area-height inner) 2))
          (widget/list state)))
      (hash "handle_event"
        (fn (state event)
          (define c (key-event-char event))
          (cond
            [(and on-confirm (or (equal? c #\y) (equal? c #\Y)))
             (on-confirm)
             (close!)]
            [(or (equal? c #\n) (equal? c #\N) (key-event-escape? event))
             (close!)]
            [else event-result/consume])))))
  (set-box! *preview-open* #t)
  (push-component! component))

