;; I wrote a tiny unused code deletion thingy to be able to just put tons of defuns here
;; and attach, then optimize them out of every rom. this seems to be the most user-friendly thing to do
;; Another thing that crossed my mind was to automatically detect undefined, but define-able functions
;; and compile them during (finalize), but that seems much more complicated than this

(define-macro-rule ()
  (defun f (arg1 . args) . body)
  (_defun f (arg1 . args) body))

(define-macro-rule ()
  (defun f (arg1) . body)
  (_defun f (arg1) body))

(define-macro-rule ()
  (defun f () . body)
  (_defun f () body))

(define-macro-rule ()
  (define (f . args) . body)
  (_defun f args body))

(define-macro-rule () ; TODO: (a . b) doesn't capture b when b is empty
  (define (f) . body)
  (_defun f () body))

(define-macro-rule ()
  (define-vector (f . args) . body)
  (_defun-vector f args body))

(define-macro-rule ()
  (define-vector (f) . body)
  (_defun-vector f () body))

(define-macro-rule ()
  (push! it)
  (_push! _ it))

(define-macro-rule ()
  (pus! it)
  (_push! byte it))

(define-macro-rule ()
  (deo!)
  (uxn-call! () deo))

(define-macro-rule ()
  (deo2!)
  (uxn-call! (2) deo))

(define-macro-rule ()
  (dei!)
  (uxn-call! () dei))

(define-macro-rule ()
  (dei2!)
  (uxn-call! (2) dei))

(define-macro-rule ()
  (brk!)
  (uxn-call! () brk))

(define-macro-rule ()
  (short->byte x)
  (_push! byte x))

(define-macro-rule ()
  (alloc! name . vals)
  (_alloc! name vals))

(define-macro-rule ()
  (deo-with arg at)
  (begin
    (pus! arg)
    (pus! at)
    (deo!)))

(define-macro-rule ()
  (deo2-with arg at)
  (begin
    (push! arg)
    (pus! at)
    (deo2!)))

(define-macro-rule ()
  (define-simple-deo name addr)
  (define-macro-rule ()
    (name _arg)
    (deo-with _arg addr)))

(define-macro-rule ()
  (define-simple-deo2 name addr)
  (define-macro-rule ()
    (name _arg)
    (deo2-with _arg addr)))

(define-constant color-1 0)
(define-constant color-2 1)
(define-constant color-3 2)
(define-constant color-4 3)

(define-constant fill-mode  #b10000000)
(define-constant pixel-mode #b00000000)
(define-constant layer-0    #b00000000)
(define-constant layer-1    #b01000000)
(define-constant flip-x     #b00100000)
(define-constant flip-y     #b00010000)

(codegen-at! #x100)

(main)
(uxn-call! () brk)

(define-label! nigeb) ; yup! that's how i declare a begin it uses the fact that any label will be treated
(uxn-call! (2 r) jmp) ; as a function with any amount of args; so it is a "function" without a function prelude and epilogue

(define-macro-rule (_)
  (_reverse a . b)
  (_reverse (_ . b) a))

(define-macro-rule (_)
  (_reverse (_) . a)
  a)

(define-macro-rule (_)
  (_reverse (_ b . c) . a)
  (_reverse (_ . c) b . a))

(define-macro-rule (_)
  (begin . exp)
  (nigeb . (_reverse . exp)))

(codegen-at! #x110) ; TODO: actually calculate where to codegen after prelude

(define-simple-deo putchar #x18)

(define-macro-rule ()
  (debug!)
  (begin (pus! 1) (pus! #x0e) (deo!)))

(define (+ a b)
  (push! a) ; i manually push here for the uxn-call! primitive
  (push! b)
  (uxn-call! (2) add))

(define (- a b)
  (push! b)
  (push! a)
  (uxn-call! (2) sub))

(define-macro-rule ()
  (bior* a b . c)
  (bior a (bior* b . c)))

(define-macro-rule ()
  (bior* a b)
  (bior a b))

(define (bior a b)
  (push! a)
  (push! b)
  (uxn-call! (2) ora))

(define (band a b)
  (push! a)
  (push! b)
  (uxn-call! (2) and))

(define (band8 a b)
  (pus! a)
  (pus! b)
  (uxn-call! () and))

(define (<< a b)
  (push! a)

  (pus! b)
  (pus! 4)
  (uxn-call! () sft)

  (uxn-call! (2) sft))

(define (>> a b)
  (push! a)
  (band8 b #b00001111)
  (uxn-call! (2) sft))

(define-simple-deo exit #x0f)

;; short equal
(define (equ? a b)
  (push! a)
  (push! b)
  (uxn-call! (2) equ))

;; byte equal
(define (eq? a b)
  (pus! a)
  (pus! b)
  (uxn-call! () equ))

(define-simple-deo2 set-draw-handler! #x20)
(define-simple-deo2 set-color-r! #x08)
(define-simple-deo2 set-color-g! #x0a)
(define-simple-deo2 set-color-b! #x0c)

(define (set-colors! r g b)
  (set-color-r! r)
  (set-color-g! g)
  (set-color-b! b))

(define-simple-deo2 pick-x! #x28)
(define-simple-deo2 pick-y! #x2a)

(define (pick-pixel! x y)
  (pick-x! x)
  (pick-y! y))

(define (pixel! x y color fill layer fx fy)
  (pick-pixel! x y)

  (bior* color fill layer fx fy)

  (allocate-local! mask) ; this sucks, TODO: fix the macroexpander
  (short->byte mask)

  (pus! #x2e)
  (deo!)
  (free-locals! 1))

(define (fill! x y color layer)
  (pixel! x y color fill-mode layer 0 0))

(define (draw-pixel! x y color)
  (pixel! x y color 0 0 0 0))

(define-simple-deo2 pick-sprite! #x2c)

(define (sprite! x y sprite bpp2? layer fx fy color)
  (pick-pixel! x y)
  (pick-sprite! sprite)

  (bior* bpp2? layer fx fy color)

  (allocate-local! mask) ; this sucks, TODO: fix the macroexpander
  (short->byte mask)

  (pus! #x2f)
  (deo!)
  (free-locals! 1))

(define (mouse-x)
  (pus! #x92)
  (dei2!))

(define (mouse-y)
  (pus! #x94)
  (dei2!))
