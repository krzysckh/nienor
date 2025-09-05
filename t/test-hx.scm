(_declare-test
 input  => "68 65 6c 6c 6f"
 output => "hello")

(defvar *had-nibble* 0)
(defvar *nibble-was* 0)

(define (main)
  (set-console-handler! *console-handler*))

(define-vector (*console-handler*)
  (let ((v* (console-read))
        (t  (console-type)))
    (let ((v (if (>= v* #\a) (- v* (- #\a 10)) (- v* #\0))))
      (cond
       ((= t 4)
        (exit!))
       ((= v* #\space)
        ;; skip
        )
       (*had-nibble*
        (set! *had-nibble* 0)
        (putchar (bior (<< *nibble-was* 4) v)))
       (else
        (set! *had-nibble* 1)
        (set! *nibble-was* v))))))

(metadata! "read hex data from stdin and dump as binary data to stdout" #\newline)
