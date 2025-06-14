(_declare-test
 output => "ok\nok\nok\nok\n")

(defvar *ok* "ok\n" 0)
(defvar *bad* "bad\n" 0)

(define (test res ok)
  (puts-static (if (equ? res ok) *ok* *bad*)))

(define (main)
  (test (<< 2137 2) 8548)
  (test (<< 33 7) 4224)
  (test (>> 2137 7) 16)
  (test (>> 0 7) 0)
  (exit 128))
