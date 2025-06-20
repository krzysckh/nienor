(_declare-test
 output => "ok")

(define (make x)
  (λ () x))

(alloc! *ok* #\o #\k 0)
(alloc! *bad* #\b #\a #\d 0)

(define (main)
  (let* ((n1 (with (make 10) as f
               (malloc 100)
               f))
         (_ (malloc 105))
         (n2 (with (make 20) as f2
               (malloc 100)
               f2)))
    (if (equ? n1 n2)
        (puts-static *ok*)
        (puts-static *bad*))
    (exit!)))
