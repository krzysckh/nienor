(define (app f)
  (f #\n #\k))

(define (main)
  (app (λ (a b)
         (putchar ((λ (c) (+ c ((λ (x) x) 1))) a))
         (putchar b)
         (putchar #\newline)))
  (exit 128))

;; We don't have closures though. sorry.
;;
;; (define (fuck a)
;;   (λ (b) (+ a b)))
;;
;; will error out with:
;; error: (couldn't resolve symbol  a)
