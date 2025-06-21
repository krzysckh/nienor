(_declare-test
 output => "hello scheme")

(define (main)
  (with (string-append "hello" " scheme") as s
    (puts s))
  (exit!))
