(_declare-test
 output => "ok\nok\nok\nok\n")

(define (test res ok)
  (puts (if (equ? res ok) "ok\n" "bad\n")))

(define (main)
  (test (<< 2137 2) 8548)
  (test (<< 33 7) 4224)
  (test (>> 2137 7) 16)
  (test (>> 0 7) 0)
  (exit 128))
