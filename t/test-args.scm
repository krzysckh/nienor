;; -*- Owl -*-

(_declare-test
 output => "g")

(define (f a b c d e f g)
  (putchar g))

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
     (bior8up 103 0))
  (exit #x80))
