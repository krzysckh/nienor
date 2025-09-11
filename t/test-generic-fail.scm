(_declare-test
 expected-compilation-result => failure)

(define-signature f a -> b -> b)
(define (f x y) x)

(define (main)
  (f 'a 'b))
