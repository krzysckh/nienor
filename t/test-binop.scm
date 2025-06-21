(_declare-test
 output => "aa\n")

;; TODO: fix vararg -

(define (main)
  (putchar (+ 10 10 10 10 10 10 10 10 10 7))
  (putchar (+ (* 2 4 10) 10 7))
  ;; (putchar (- 120 10 10 1 1 1))
  (putchar #\newline)
  (exit!))
