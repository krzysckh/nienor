;; -*- Owl -*-

(define-macro-rule ()
  (bior* a b . c)
  (bior a (bior* b . c)))

(define-macro-rule ()
  (bior* a b)
  (bior a b))

(define (main)
  (bior* 1 2 3 4 5 6 7 8)
  (exit!))
