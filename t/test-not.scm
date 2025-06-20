(_declare-test
 output => "0\n1\n0\n1\n")

(define (main)
  (print-number (not 1))
  (print-number (not 0))
  (print-number (not #xf000))
  (print-number (not #x0000))
  (exit!))
