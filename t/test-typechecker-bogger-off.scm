(_declare-test
 output => "oops\n")

(define-signature add2 Number -> Number -> Number)
(define (add2 a b)
  (+ a b))

(define (main)
  (let ((place (typechecker-bogger-off!
                (add2 "hello" "world"))))
    (memcpy place "oops\n" (strlen "oops\n"))
    (puts place)
    (exit!)))
