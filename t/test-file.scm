(_declare-test
 output => "pozdrowienia")

(alloc! fname "/tmp/test" 0)
(alloc! data  "pozdrowienia" 0)

(define (main)
  (write-file fname data (strlen data))
  (with (read-file fname) as buf
    (puts-static buf))
  (exit!))
