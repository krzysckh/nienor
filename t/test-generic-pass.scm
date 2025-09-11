(_declare-test
 output => "ok1\nok2\n")

(define-signature I a -> a)
(define (I x) x)

(define-signature f a -> b -> a)
(define (f x y) x)

(define (main)
  (puts (I "ok"))
  (print-number (I 1))
  (puts (f "ok" 'bad))
  (print-number (f 2 'bad))
  (exit!))
