(_declare-test
 output => "1\n")

(define-type ZeroOne)
(define-typing-rules
  (Number v is ZeroOne <=> (or
                            (= v 0)
                            (= v 1)))
  (ZeroOne  is Number))

(define-signature f ZeroOne -> Number)
(define (f x) x)

(define (main)
  (print-number (f 1))
  (exit!))
