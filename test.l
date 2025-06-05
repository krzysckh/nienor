;; -*- Owl -*-

(define (+ a b)
  (push! a) ; i manually push here for the uxn-call! primitive
  (push! b)
  (uxn-call! (2) add))

(define (- a b)
  (push! b)
  (push! a)
  (uxn-call! (2) sub))

(define (putchar c)
  (pus! c)
  (pus! #x18)
  (uxn-call! () deo))

(define (exit code)
  (pus! code)
  (pus! #x0f)
  (uxn-call! () deo))

(define (equ? a b)
  (push! a)
  (push! b)
  (uxn-call! (2) equ))

(define (eq? a b)
  (pus! a)
  (pus! b)
  (uxn-call! () equ))

(define (puts n)
  (if (equ? n 0)
      #t
      (begin
        (putchar) ; the string is on the stack, so we don't pass anything
                  ; there is no arg checking so we "can" do that
        (puts (- n 1)))))

(define (main)
  (push! "Hello, World!\n")
  (puts 14)
  (exit 128))
