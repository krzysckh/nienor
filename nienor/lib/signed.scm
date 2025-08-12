(define-type Signed)
(define-typing-rules
  (Number is Signed)
  (Bool   is Signed))


(define-signature _print-signed-number Signed -> Number -> Void)
(define (_print-signed-number n depth)
  (when (sign n)
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

(define-signature signed+ Signed -> Signed -> Signed)
(define (signed+ a b)
  (let ((a (if (sign a) (+ 1 (bxor #xffff (band #x7fff a))) a))
        (b (if (sign b) (+ 1 (bxor #xffff (band #x7fff b))) b)))
    (let ((res (+ a b)))
      (if (sign res)
          (+ 1 (bxor #xffff (band #x7fff res)))
          res))))

(define-signature signed- Signed -> Signed -> Signed)
(define (signed- a b)
  (signed+ a (bxor #x8000 b)))

(define-macro-rule ()
  (sign x)
  (band #x8000 x))

(define-signature signed* Signed -> Signed -> Signed)
(define (signed* a b)
  (bior
   (bxor (sign a) (sign b))
   (* (signed a) (signed b))))

(define-signature signed/ Signed -> Signed -> Signed)
(define (signed/ a b)
  (bior
   (bxor (sign a) (sign b))
   (/ (signed a) (signed b))))

(define-signature signed> Signed -> Signed -> Bool)
(define (signed> a b)
  (let ((sa (sign a))
        (sb (sign b)))
    ;; (print-signed-number a)
    ;; (print-signed-number b)
    ;; (print-number sa)
    ;; (print-number sb)
    (if (equ? sa sb)
        (if sa (< a b) (> a b))
        (if (and sa (not sb)) #f #t))))

(define-macro-rule ()
  (signed< a b)
  (not (signed> a b)))

(define-macro-rule ()
  (signed<= a b)
  (bior (signed< a b) (= a b)))

(define-macro-rule ()
  (signed>= a b)
  (bior (signed> a b) (= a b)))

(define-macro-rule ()
  (signed x)
  (bxor #x8000 (negative! x)))

(define-macro-rule ()
  (negative! x)
  (bior #x8000 x))

(define-macro-rule ()
  (negative? x)
  (if (band #x8000 x) #t #f))

(define-macro-rule ()
  (positive? x)
  (not (negative? x)))

(define-macro-rule ()
  (signed-min2 a b)
  (if (signed< a b) a b))

(define-macro-rule ()
  (signed-max2 a b)
  (if (signed> a b) a b))

(define-signature number->signed Number -> Signed)
(define (number->signed x)
  (signed x))
