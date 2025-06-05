(alloc!
 test-string
 "abcd"
 "efgh"
 #\i #\j #\newline 0)

(define (puts ptr)
  (push! ptr)
  (uxn-call! () lda)
  (pus! 0)
  (uxn-call! () swp)
  (allocate-local! top)
  (if (equ? top 0)
      #t
      (begin
        (putchar top)
        (puts (+ ptr 1))))
  (free-locals! 1))

(define (main)
  (puts test-string)
  (exit #x80))
