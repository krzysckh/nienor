(_declare-test
 output => "hello\n")

(define (returns-2)
  2)

(define (main)
  (putchar (+ #\f 2))
  (putchar (+ #\c (returns-2)))
  (putchar (- #\n 2))
  (putchar (- #\o 3))
  (putchar (+ #\o 0))
  (putchar #\newline)
  (exit 128))

;; (define (main)
;;   (loopn (i 0 10 1)
;;     (putchar (+ #\0 i))))
