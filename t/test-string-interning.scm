(_declare-test
 output => "ok")

(define (main)
  (if (and
       (= "hello1" "hello1")
       (= "hello2" "hello2")
       (= "hello3" "hello3")
       (= "hello4" "hello4"))
      (puts "ok")
      (puts "bad"))
  (exit!))
