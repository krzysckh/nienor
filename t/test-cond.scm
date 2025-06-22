(_declare-test
 output => "okokok")

(define (main)
  (let ((x "pozdrawiam"))
    (cond
     ((string=? x "pozdrawiam")
      (puts "ok"))
     (else
      (puts "bad"))))

  (let ((v 2))
    (puts
     (cond
      ((< v 2) "bad")
      ((> v 2) "also bad")
      ((= v 2) "ok"))))

  (let* ((x 0)
         (res (cond
               ((equ? x 10) "bad")
               ((equ? x 12) "bad")
               (else        "ok"))))
    (puts res))
  (exit!))
