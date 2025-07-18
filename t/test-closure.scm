(_declare-test
 output => "abcd\n6\n42\n2137\n420\n42\n")

(define (make i)
  (λ (x)
    (+ i x)))

(define (make-add3 a)
  (λ (b)
    (λ (c)
      (+ a b c))))

(define (check-args a b)
  (λ (c d)
    (λ ()
      (putchar a)
      (putchar b)
      (putchar c)
      (putchar d)
      (putchar #\newline))))

(define (main)
  (((check-args #\a #\b) #\c #\d))
  (let ((add3 ((make-add3 1) 2)))
    (print-number (add3 3))
    (print-number (add3 39)))

  (let ((funny-inator-1 (make 41))
        (funny-inator-2 (make 2136))
        (funny-inator-3 (make 419)))
    (print-number (funny-inator-2 1))
    (print-number (funny-inator-3 1))
    (print-number (funny-inator-1 1)))

  (exit 128))
