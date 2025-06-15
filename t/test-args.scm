;; -*- Owl -*-

(_declare-test
 output => "g")

(define (f a b c d e f g)
  (putchar g))

(define (main)
  (debug!)
  (f #\a #\b #\c #\d #\e #\f #\g)
  (exit #x80))
