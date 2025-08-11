(_declare-test
 output => "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")

;; "hard" because of additional locals
(define-signature tc Number -> Void)
(define (tc a)
  (let ((b 0))
    (let ((c b))
      (if (equ? a 0)
          (noop)
          (begin
            (putchar #\a)
            (tc (- a 1)))))))

(define (main)
  (tc 255)
  (exit!))
