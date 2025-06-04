; -*- Owl -*-

;; (define a 10) ; <- this will get inlined

(define (+ a b)
  (uxn-call! add (a b)))

(define (*2 a)
  (uxn-call! mul (a 2)))

(define (3rd a b c)
  c)

; ->

(define-label! +)
(allocate-local! a)
(allocate-local! b)
(pop-to-local! b)
(pop-to-local! a)
(push-local! a)
(push-local! b)
(uxn-call! 'add)
(free-locals! 2)
(uxn-call2r! 'jmp)

(define-label! _3rd)
(allocate-local! a) ; locals would be stored in the zero-page
(allocate-local! b) ; with a pointer to the last stored local at #0
(allocate-local! c) ; (a makeshift 3rd stack)
(pop-to-local! c)
(pop-to-local! b)
(pop-to-local! a)
(push-local! c)
(free-locals! 3)
(uxn-call2r! 'jmp)

(define (main)
  (+ 10 20))

;; |0100
;; @fun__plus_  ( a b -> ? )
;;   ADD
;;   JMP2r
;;
;; @fun__plus_2 ( a2 b2 -> ?2 )
;;   ADD2
;;   JMP2r
;;
;; @main
;;   #0a #14 !fun__plus_

;; which would be compiled to
;; (at 0100)
;; ADD JMP2r  ( @fun__plus_ )
;; ADD2 JMP2r ( @fun__plus_2 )
;; #0a #14 #0100 JSR2 BRK
