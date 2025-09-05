(_declare-test
 output => "aaa\n")

(define (main)
  (putchar (+ 10 10 10 10 10 10 10 10 10 7))
  (putchar (+ (* 2 4 10) 10 7))
  (putchar (- 120 10 10 1 1 1))
  (putchar #\newline)
  (exit!))
