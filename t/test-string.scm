(_declare-test
 output => "Hello, World!\n")

(defvar hello "Hello, World!\n" 0)

(define (main)
  (let ((puts puts-static))
    (puts hello)
    (exit 128)))
