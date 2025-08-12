(_declare-test
 output => "ok")

(define (main)
  (let ((s1 "hello1")
        (s2 "hello1")
        (s3 "hello2")
        (s4 "hello2")
        (s5 "hello3")
        (s6 "hello3"))
    (if (and
         (= s1 s2)
         (= s3 s4)
         (= s5 s6))
        (puts "ok")
        (puts "bad"))
    (exit!)))
