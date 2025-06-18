(_declare-test
 output => "Hello, World!\n")

(defvar hello "Hello, World!\n" 0)

(define (main)
  (puts-static (addrof hello))
  (exit 128))
