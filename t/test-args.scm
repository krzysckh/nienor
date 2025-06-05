;; -*- Owl -*-

(define (debug!)
  (pus! 1)
  (pus! #x0e)
  (deo!))

(define (putchar c)
  (pus! c)
  (pus! #x18)
  (uxn-call! () deo))

(define (f a b c d e f g)
  (putchar g))

(define (+ a b)
  (push! a) ; i manually push here for the uxn-call! primitive
  (push! b)
  (uxn-call! (2) add))

(define (bior8 a b)
  (pus! a)
  (pus! b)
  (uxn-call! () ora))

(define (bior8up a b)
  (pus! 0)
  (bior8 a b))

(define (main)
  (f (bior8up 97 0)
     (bior8up 98 0)
     (bior8up 99 0)
     (bior8up 100 0)
     (bior8up 101 0)
     (bior8up 102 0)
     (bior8up 103 0)))
