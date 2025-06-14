(_declare-test
 output => "ok\n")

(define (app f)
  (f #\n #\k))

(define (app* f)
  (λ (x) (f x)))

(define (main)
  (app (λ (a b)
         (putchar ((λ (c) (+ c ((λ (x) x) 1))) a))
         (putchar b)))
  ((app* (λ (c) (putchar c))) (- ((fuck #\newline) 32) 32))
  (exit 128))

(define (fuck a)
  (λ (b) (+ a b)))
