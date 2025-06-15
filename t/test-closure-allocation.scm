(_declare-test
 output => "ok")

(define (make n)
  (λ () n))

(alloc! ok "ok" 0)
(alloc! bad "bad" 0)

(define (main)
  (let ((f1 (make 0))
        (f2 (make 2)))
    (free f1)
    (if (equ? f1 (make 3))
        (puts-static ok)
        (puts-static bad))
    (exit!)))
