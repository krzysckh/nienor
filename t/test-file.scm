(_declare-test
 output => "pozdrowienia")

(alloc! data "pozdrowienia" 0)

(define-signature ptr->str Pointer -> String)
(define (ptr->str b) b)

(define (main)
  (let ((fname "/tmp/test")
        (p (malloc (+ 1 (strlen data)))))
    (memcpy p data (strlen data))
    (write-file fname p (strlen data))
    (with (read-file fname) as buf
      (puts (ptr->str buf)))
    (exit!)))
