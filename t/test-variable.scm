(_declare-test
 output => "2137\n21\n42\n0\n")


;; Also consider making variables 1st class citizens, known about
;; by the compiler, so one could potentially define it with a name
;; and later implicitly insert get! into places where it knows a variable
;; and not a label nor a constant would be
;; ^-- this is done. this test got restructured to test exactly that.

(defvar *variable-a* 2137)
(defvar *variable-b* 21)
(defvar *variable-c* 42)
(alloc! *not-a-variable* 0 33)

(define (main)
  (set! *variable-b* 21)
  (print-number *variable-a*)
  (print-number *variable-b*)
  (print-number *variable-c*)
  (print-number (equ? 33 *not-a-variable*))

  (exit 128))
