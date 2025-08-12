(_declare-test
 expected-compilation-result => failure)

(define-type ZeroOne)
(define-typing-rules
  (Number v is ZeroOne <=> (or
                            (= v 0)
                            (= v 1)))
  (ZeroOne  is Number))

(define-signature f ZeroOne -> Number)
(define (f x) x)

(define (main)
  (f 3))
