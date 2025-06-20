(_declare-test
 output => "1\n0\n0\n")

(define (main)
  (print-number (and 1 2 3 4 5 6 7 8 9 10))
  (print-number (and 1 7 0))
  (print-number (and 0 0 0 0 0 0))
  (exit!))
