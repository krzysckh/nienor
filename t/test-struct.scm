(define-struct point _x _y get-x get-y set-x set-y)

;; OOP: oats oriented programming
;; these are fricking custom {set,get}ters. i'm so sorry.
;; this is just made to check that everything works correctly. please don't do that in actual code.
;; seconds seperate us from Microsoft Visual Uxn
(define (make-point*)
  (let ((pt (make-point)))
    (set-point-get-x! pt (λ () (point-_x pt)))
    (set-point-get-y! pt (λ () (point-_y pt)))
    (set-point-set-x! pt (λ (x) (set-point-_x! pt x))) ; closures! fancy!
    (set-point-set-y! pt (λ (y) (set-point-_y! pt y)))
    pt))

;; normally, you'd
;; (define-struct point x y)
;; and
;;
;; (with (make-point) as pt
;;   (set-point-x! pt 10)
;;   (do-something-with (point-x pt)))
;;
;; if you wish to use a struct outside of a (with ...) scope, just declare it in a let-expression, but remember
;; there's no GC, so it will linger somewhere in the memory
;;
;; (let ((pt (make-point)))
;;   (set! some-outside-variable pt))

(define (main)
  ;; TODO: resolve this at runtime rather than compile-time. (make-instance should be a function, not a silly macro)
  (with (make-instance 'point) as pt
    (print-number pt)) ; immidiately free it afterwards
  (with (make-point*) as pt
    (with (make-point*) as pt2
      (set-point-_y! pt2 2138)
      (let ((get-x (point-get-x pt))
            (get-y (point-get-y pt))
            (set-x (point-set-x pt))
            (set-y (point-set-y pt)))
        (set-x 10)
        (set-y 20)
        (print-number (get-x))
        (print-number (get-y))
        (set-y 42)
        (print-number (get-y))
        (print-number ((point-get-y pt2)))
        )))
  (exit!))
