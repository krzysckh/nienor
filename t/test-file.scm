(_declare-test
 output => "pozdrowienia")

(alloc! data "pozdrowienia" 0)

(define (main)
  (let ((fname "/tmp/test")
        (p (malloc (+ 1 (strlen data)))))
    (memcpy p data (strlen data))
    (write-file fname p (strlen data))
    (with (read-file fname) as buf
      (puts buf))
    (exit!)))
