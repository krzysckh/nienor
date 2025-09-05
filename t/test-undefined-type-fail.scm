(_declare-test
 expected-compilation-result => failure)

(define-signature f Unknown -> Undefined)
(define (I x) x)

(define (main)
  (I 10))
