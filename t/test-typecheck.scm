(define-signature plus2 Number -> Number)
(define (plus2 a)
  (+ 2 a))

(define-signature app String -> String)
(define (app a)
  (string-append a "b"))

(define-signature main Void)
;; (define (main)
;;   (plus2 (app "dupa"))
;;   (exit!))

(define (main)
  "well i hope it works" ;; TODO: fix run-tests so it can accept a compiler failure as a passed test
  (exit!))
