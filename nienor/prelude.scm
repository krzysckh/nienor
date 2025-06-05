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
  (define (f . arg) . body)
  (_defun f arg body))

(define-macro-rule ()
  (define-vector (f . args) . body)
  (_defun-vector f args body))

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
    arg (uxn-call! () nip)
    at  (uxn-call! () nip)
    (deo!)))

(define-macro-rule ()
  (deo2-with arg at)
  (begin
    arg at (uxn-call! () nip) (deo2!)))

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

(define-macro-rule ()
  (define-binop name uxn-op . modes)
  (define-macro-rule ()
    (name _arg1 _arg2)
    (begin
      _arg1 _arg2
      (uxn-call! modes uxn-op))))

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

;;;---
;; this gets called at #x100, this should contain only the most important stuff
;; to bootstrap the execution
;; that's why the compiler requires (main) to be defined, it's like _start in assemblers
;; ...or main in C
;; ...or WinMain in microsoft C

(main)
(brk!)

;;---

;; now jump with code generation to #x110 (arbitrary number after calling #x100,
;; enough to hold the bootstrap part)
(codegen-at! #x110) ; TODO: actually calculate where to codegen after prelude


;; I define begin with no special syntax treatment
;; it's a hack as follows, i declare a `nigeb' label, which is begin backwards
;; it (as the name suggests) does what begin would do backwards
;; that's because the compiler thinks it's a function, and tries to (_push!)
;; the results of functions that are arguments... in reverse order
;; that's why we have a _reverse macro, which does a nigeb->begin transformation
;; (reverses arguments) - now we can create a begin !

;; this is a function that just returns
(define-label! nigeb)
(uxn-call! (2 r) jmp)

(define-macro-rule ()
  (_reverse)
  ())

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

(define-simple-deo putchar #x18)
(define-simple-deo putchar-error #x19)

(define-macro-rule ()
  (debug!)
  (begin (pus! 1) (pus! #x0e) (deo!)))

(define-macro-rule ()
  (when exp . body)
  (if exp (begin . body) (begin)))

(define-binop + add 2)
(define-binop - sub 2)
(define-binop * mul 2)
(define-binop / div 2)
(define-binop band and 2)
(define-binop bior ora 2)
(define-binop bxor eor 2)

(define-macro-rule ()
  (> a b)
  (begin a b (uxn-call! (2) gth) (pus! 0) (uxn-call! () swp)))

(define-macro-rule ()
  (< a b)
  (begin a b (uxn-call! (2) lth) (pus! 0) (uxn-call! () swp)))

(define-macro-rule ()
  (bior* a b . c)
  (bior a (bior* b . c)))

(define-macro-rule ()
  (bior* a b)
  (bior a b))

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
(define-simple-deo2 set-mouse-handler! #x90)

(define-simple-deo2 set-color-r! #x08)
(define-simple-deo2 set-color-g! #x0a)
(define-simple-deo2 set-color-b! #x0c)

(define-simple-deo2 set-screen-width! #x22)
(define-simple-deo2 set-screen-height! #x24)

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

(define (mouse-state)
  (pus! #x96)
  (dei!)
  (pus! 0)
  (uxn-call! () swp))

(define (get! addr)
  (push! addr)
  (uxn-call! (2) lda))

(define (set! addr value)
  (push! value)
  (push! addr)
  (uxn-call! (2) sta))

(define (get8! addr)
  (push! addr)
  (uxn-call! () lda)
  (pus! 0)
  (uxn-call! () swp))

(define (set8! addr value)
  (pus! value)
  (push! addr)
  (uxn-call! () sta))

(define-macro-rule ()
  (with-label label . body)
  (_with-label label body))

(define-macro-rule ()
  (jmp! to)
  (begin
    to (uxn-call! (2) jmp)))

(define-macro-rule ()
  (loopn (it from to by) . body)
  (begin
    to from
    (with-label _loop
      (uxn-call! (2) dup)
      (allocate-local! it)
      (begin . body)
      (free-locals! 1)
      (+ by (begin)) ; from += by ; begin as i don't have any way to say "take from the top of the stack" lmao!
      (if (uxn-call! (k 2) gth)
          (jmp! _loop)
          (begin
            (uxn-call! (2) pop)
            (uxn-call! (2) pop))))))

(define-macro-rule ()
  (with-local k v . body)
  (begin
    v (allocate-local! k)
    (begin . body)
    (free-locals! 1)))

(define-macro-rule ()
  (let ((key val) . rest) . body)
  (with-local key val (let rest . body)))

(define-macro-rule ()
  (let ((key val)) . body)
  (with-local key val . body))
