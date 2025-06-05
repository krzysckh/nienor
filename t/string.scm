(alloc!
 test-string
 "abcd"
 "efgh"
 #\i #\j 0)

(define (putchar c)
  (pus! c)
  (pus! #x18)
  (uxn-call! () deo))

(define (+ a b)
  (push! a) ; i manually push here for the uxn-call! primitive
  (push! b)
  (uxn-call! (2) add))

(define (equ? a b)
  (push! a)
  (push! b)
  (uxn-call! (2) equ))

(define (debug!)
  (pus! 1)
  (pus! #x0e)
  (deo!))

(define (puts ptr)
  (push! ptr)
  (uxn-call! () lda)
  (pus! 0)
  (uxn-call! () swp)
  (allocate-local! top)
  (if (equ? top 0)
      #t
      (begin
        (puts (+ ptr 1))
        (putchar top)))
  (free-locals! 1))

(define (main)
  (puts test-string)
  )
