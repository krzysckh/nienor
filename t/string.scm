(alloc! hello "Hello, World!\n" 0)

(define (puts ptr)
  (if (equ? (get8! ptr) 0)
      #t
      (begin
        (putchar (get8! ptr))
        (puts (+ ptr 1)))))

(define (main)
  (puts hello)
  (exit 128))
