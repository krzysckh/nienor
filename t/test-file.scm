(_declare-test
 output => "pozdrowienia")

(alloc! data "pozdrowienia" 0)

(define (main)
  (let ((fname "/tmp/test"))
    (write-file fname "pozdrowienia" (strlen data))
    (with (read-file fname) as buf
      (puts buf))
    (exit!)))
