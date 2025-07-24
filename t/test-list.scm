(_declare-test
 output => "()\n(2 1 3 7)\n(1 3 7)\n(3 7)\n(7)\n")

(define (main)
  (print-list nil)
  (let loop ((lst (list 2 1 3 7)))
    (when lst
      (print-list lst)
      (loop (cdr lst))))
  (exit!))
