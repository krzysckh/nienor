(_declare-test
 output => "1234")

(defvar var)
(alloc! v 0 0)

(define (main)
  (set! var 1)
  (print-number* var)
  (set! v 2)
  (print-number* (get! v))
  (let ((n 3))
    (print-number* n)
    (set! n 4)
    (print-number* n))
  (exit!))
