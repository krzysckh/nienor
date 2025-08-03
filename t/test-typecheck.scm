(_declare-test
 expected-compilation-result => failure)

(define-signature plus2 Number -> Number)
(define (plus2 a)
  (+ 2 a))

(define-signature app String -> String)
(define (app a)
  (string-append a "b"))

(define-signature main Void)

(define (main)
  (print (app (plus2 2)))
  (exit!))
