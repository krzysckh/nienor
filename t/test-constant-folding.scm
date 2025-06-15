(_declare-test
 output => "aaaa")

(define blah (+ 10 2)) ; this works *only* when compiled with -O

(define (main)
  (putchar (+ blah 85))
  (putchar (+ 90 7))
  (putchar (- 100 3))
  (putchar (/ (/ (* 97 4 2) 4) 2))
  (exit 128))
