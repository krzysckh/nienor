(_declare-test
 output => "-1\n1\n1\n-1\n-1\n1\n1\n-1\n-10\n")

(define (_print-signed-number n depth)
  (when (band #x8000 n)
    (putchar #\-))
  (if (equ? 1 (band (equ? n 0) (> depth 0)))
      (noop)
      (let ((n (band #x7fff n)))
        (_print-signed-number (/ n 10) (+ depth 1))
        (putchar (+ #\0 (modulo n 10))))))

(define-macro-rule ()
  (print-signed-number n)
  (begin
    (_print-signed-number n 0)
    (putchar #\newline)))

(define (signed+ a b)
  (let ((sign-a (band #x8000 a))
        (sign-b (band #x8000 b)))
    (let ((a (if sign-a (+ 1 (bxor #xffff (band #x7fff a))) a))
          (b (if sign-b (+ 1 (bxor #xffff (band #x7fff b))) b)))
      (let ((res (+ a b)))
        (if (band #x8000 res)
            (+ 1 (bxor #xffff (band #x7fff res)))
            res)))))

(define (signed- a b)
  (signed+ a (bxor #x8000 b)))

(define (main)
  (print-signed-number (signed+ 10 (bior #x8000 11)))
  (print-signed-number (signed+ 10 (bior #x8000 9)))
  (print-signed-number (signed+ (bior #x8000 10) 11))
  (print-signed-number (signed+ (bior #x8000 10) 9))

  (print-signed-number (signed- 10 11))
  (print-signed-number (signed- 10 9))
  (print-signed-number (signed- (bior #x8000 10) (bior #x8000 11)))
  (print-signed-number (signed- (bior #x8000 10) (bior #x8000 9)))

  (print-signed-number (signed- 0 10))

  (exit 128))
