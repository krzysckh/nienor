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
  (define name value)
  (define-constant name value))

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
  (λ args . body)
  (_λ args body))

(define-macro-rule (())
  (λ () . body)
  (_λ () body))

(define-macro-rule ()
  (lambda args . body)
  (λ args . body))

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
  (byte->short x)
  (begin
    x (pus! 0) (uxn-call! () swp)))

(define-macro-rule ()
  (alloc! name . vals)
  (_alloc! name vals))

(define-macro-rule ()
  (defvar name . vs)
  (alloc! name . vs))

(define-macro-rule ()
  (defvar name)
  (alloc! name 0 0))

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
  (flatten!
   (define-macro-rule ()
     (name _arg1 . argn)
     (name _arg1 (name . argn)))

   (define-macro-rule ()
     (name _arg1 _arg2)
     (begin
       _arg1 _arg2
       (uxn-call! modes uxn-op)))))

(define-macro-rule ()
  (with-label label . body)
  (_with-label label body))

(define-macro-rule ()
  (_declare-test . blah)
  (_flatten! ()))

(define-macro-rule ()
  (include! filename)
  (_include! filename))

(define color-1 0)
(define color-2 1)
(define color-3 2)
(define color-4 3)

(define fill-mode  #b10000000)
(define pixel-mode #b00000000)
(define layer-0    #b00000000)
(define layer-1    #b01000000)
(define flip-x     #b00100000)
(define flip-y     #b00010000)

(define bg-dot     #x00)
(define bg-fill-br #x80)
(define bg-fill-bl #x90)
(define bg-fill-tr #xa0)
(define bg-fill-tl #xb0)
(define fg-dot     #x40)
(define fg-fill-br #xc0)
(define fg-fill-bl #xd0)
(define fg-fill-tr #xe0)
(define fg-fill-tl #xf0)

(codegen-at! #x100)

;;;---
;; this gets called at #x100, this should contain only the most important stuff
;; to bootstrap the execution
;; that's why the compiler requires (main) to be defined, it's like _start in assemblers
;; ...or main in C
;; ...or WinMain in microsoft C

(malloc/init)
(main)
(brk!)

(defvar *rt-finish* *compiler-end*)

;;---


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

;; TODO: error: (couldn't match (and) to any rewrite rules)
;; TODO: make it possible for macros to somehow yield multiple toplevel exps OR search for macro-rules deeper than just toplevel o_O
;; (define-macro-rule ()
;;   (or a b)
;;   (bior a b))

;; (define-macro-rule ()
;;   (xor a b)
;;   (bxor a b))

;; (define-macro-rule ()
;;   (and a b)
;;   (band a b))

(define-macro-rule ()
  (> a b)
  (begin a b (uxn-call! (2) gth) (pus! 0) (uxn-call! () swp)))

(define-macro-rule ()
  (< a b)
  (begin a b (uxn-call! (2) lth) (pus! 0) (uxn-call! () swp)))

(define-macro-rule ()
  (>= a b)
  (bior (> a b) (equ? a b)))

(define-macro-rule ()
  (<= a b)
  (bior (< a b) (equ? a b)))

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
  (pus! #x40)
  (uxn-call! () sft)

  (uxn-call! (2) sft))

(define (>> a b)
  (push! a)
  (band8 b #b00001111)
  (uxn-call! (2) sft))

(define-simple-deo exit #x0f)

(define-macro-rule ()
  (flatten! . body)
  (_flatten! body))

(define-macro-rule ()
  (exit!)
  (exit 128))

;; short equal
(define (equ? a b)
  (push! a)
  (push! b)
  (uxn-call! (2) equ)
  (pus! 0)
  (uxn-call! () swp))

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

;; TODO: these should be macros
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
      ;; (debug!)
      (+ by (begin)) ; from += by ; begin as i don't have any way to say "take from the top of the stack" lmao!
      (if (byte->short (uxn-call! (k 2) gth))
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

;; (define-macro-rule (_)
;;   (_let-loop name (_ . keys) (_ . vals) (_ (key value) . rest) . body)
;;   (_let-loop name (_ key . keys) (_ value . vals) (_ . rest) . body))

;; (define-macro-rule (_)
;;   (_let-loop name (_ . keys) (_ . vals) (_) . body)
;;   (let ((name (with-label _wtf
;;                 (let ((f (λ (_reverse . keys) (jmp! _wtf))))
;;                   (begin . body)
;;                   (uxn-call! (2 r) jmp)
;;                   f))))
;;     (name . (_reverse . vals))))

;; (define-macro-rule (_)
;;   (let loop ((key val) . rest) . body)
;;   (_let-loop loop (_ key) (_ val) (_ . rest) . body))

(define-macro-rule ()
  (let ((key val) . rest) . body)
  (with-local key val (let rest . body)))

(define-macro-rule ()
  (let ((key val)) . body)
  (with-local key val . body))

(define (modulo a mod)
  (- a (* (/ a mod) mod)))

(define-macro-rule ()
  (noop)
  (begin))

;; very very inefficient and local-stack exhausting way to print a number
(define (_print-number n depth)
  (if (equ? 1 (band (equ? n 0) (> depth 0)))
      (noop)
      (begin
        ;; (debug!)
        (_print-number (/ n 10) (+ depth 1)) ; ouch!
        (putchar (+ #\0 (modulo n 10))))))

(define-macro-rule ()
  (print-number n)
  (begin
    (_print-number n 0)
    (putchar #\newline)))

(define (puts-static ptr)
  (if (equ? (get8! ptr) 0)
      #t
      (begin
        (putchar (get8! ptr))
        (puts-static (+ ptr 1)))))

;; https://github.com/krzysckh/goofing/blob/master/uxn-helpers.c
;; https://en.wikipedia.org/wiki/Xorshift
(alloc! *rand-seed-state* 8 91)

(define (rand)
  (let ((x (get! *rand-seed-state*))
        (x (bxor x (<< x 3)))
        (x (bxor x (>> x 7)))
        (x (bxor x (<< x 1))))
    (set! *rand-seed-state* x)
    x))

(define (srand x)
  (set! *rand-seed-state* x))

(define (fill-rect! sprite color x1 y1 x2 y2 layer)
  (loopn (i y1 y2 8)
    (loopn (j x1 x2 8)
      (sprite! j i sprite 0 layer 0 0 color))))

(define (vputc chr x y color layer)
  (sprite! x y (+ *atari8* (* 8 chr)) 0 layer 0 0 color))

(define (vputs ptr x y color layer)
  (let ((chr (get8! ptr)))
    (if (equ? chr 0)
        (noop)
        (begin
          (vputc chr x y color layer)
          (vputs (+ ptr 1) (+ x 8) y color layer)))))

(define-macro-rule ()
  (_after-rt! byte)
  (begin
    (set8! (get! *rt-finish*) byte)
    (set! *rt-finish* (+ *rt-finish* 1))))

(define-macro-rule ()
  (_after-rt!* short)
  (begin
    (set! (get! *rt-finish*) short)
    (set! *rt-finish* (+ *rt-finish* 2))))

(alloc! *atari8*
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x04 #x08 #x08 #x08 #x08 #x08 #x04 #x08 #x08 #x08 #x08 #x05 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x04 #x05 #x07 #x08 #x07 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x06 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x06 #x08 #x06 #x08 #x08
  #x05 #x08 #x08 #x07 #x08 #x08 #x07 #x08 #x08 #x06 #x06 #x08 #x06 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x07 #x04 #x07 #x07 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x07 #x08 #x08 #x08 #x08 #x08 #x07 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x06 #x05 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x05 #x06 #x08 #x08 #x06 #x07 #x08 #x08 #x06
  #x07 #x08 #x08 #x08 #x07 #x07 #x08 #x07 #x05 #x07 #x08 #x08 #x07 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x07 #x04 #x08 #x07 #x07 #x07 #x07
  #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x18 #x3c #x66 #xc3 #xe7 #x24 #x24 #x3c
  #x3c #x24 #x24 #xe7 #xc3 #x66 #x3c #x18 #x18 #x1c #xf6 #x83 #x83 #xf6 #x1c #x18
  #x18 #x38 #x6f #xc1 #xc1 #x6f #x38 #x18 #x3c #x99 #xc3 #xe7 #xc3 #x99 #x3c #x00
  #xff #xff #xfe #xfc #xf9 #xf3 #xe7 #x00 #xe7 #xc3 #x99 #x3c #x99 #xc3 #xe7 #x00
  #x01 #x03 #x06 #x8c #xd8 #x70 #x20 #x00 #x7e #xc3 #xd3 #xd3 #xdb #xc3 #xc3 #x7e
  #x18 #x3c #x3c #x3c #x7e #x10 #x38 #x10 #x18 #x1c #x16 #x10 #x10 #x70 #xf0 #x60
  #xf0 #xc0 #xfe #xd8 #xde #x18 #x18 #x00 #xf0 #xc0 #xdf #xdb #xff #x1e #x1b #x00
  #x05 #x05 #x05 #x0d #x0d #x19 #x79 #x71 #xa0 #xa0 #xa0 #xb0 #xb0 #x98 #x9e #x8e
  #x7c #xc6 #xc6 #x00 #xc6 #xc6 #x7c #x00 #x06 #x06 #x06 #x00 #x06 #x06 #x06 #x00
  #x7c #x06 #x06 #x7c #xc0 #xc0 #x7c #x00 #x7c #x06 #x06 #x7c #x06 #x06 #x7c #x00
  #xc6 #xc6 #xc6 #x7c #x06 #x06 #x06 #x00 #x7c #xc0 #xc0 #x7c #x06 #x06 #x7c #x00
  #x7c #xc0 #xc0 #x7c #xc6 #xc6 #x7c #x00 #x7c #x06 #x06 #x00 #x06 #x06 #x06 #x00
  #x7c #xc6 #xc6 #x7c #xc6 #xc6 #x7c #x00 #x7c #xc6 #xc6 #x7c #x06 #x06 #x7c #x00
  #x00 #x00 #x3c #x06 #x7e #x66 #x3c #x00 #x78 #x60 #x78 #x60 #x7e #x18 #x1e #x00
  #x07 #x0f #x1f #x18 #x18 #x10 #x1e #x17 #xf0 #xf8 #xec #x04 #x04 #x04 #x3c #x54
  #x11 #x0b #x0d #x06 #x07 #x2e #x39 #x38 #x04 #x28 #xd8 #x28 #xd0 #x10 #xe0 #x00
  #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x60 #x60 #x60 #x60 #x60 #x00 #x60 #x00
  #x66 #x66 #x66 #x00 #x00 #x00 #x00 #x00 #x00 #x6c #xfe #x6c #x6c #xfe #x6c #x00
  #x18 #x3e #x60 #x3c #x06 #x7c #x18 #x00 #x00 #x66 #x6c #x18 #x30 #x66 #x46 #x00
  #x38 #x6c #x38 #x70 #xde #xcc #x76 #x00 #x60 #x60 #x60 #x00 #x00 #x00 #x00 #x00
  #x0e #x1c #x18 #x18 #x18 #x1c #x0e #x00 #x70 #x38 #x18 #x18 #x18 #x38 #x70 #x00
  #x00 #x66 #x3c #xff #x3c #x66 #x00 #x00 #x00 #x18 #x18 #x7e #x18 #x18 #x00 #x00
  #x00 #x00 #x00 #x00 #x00 #x30 #x30 #x60 #x00 #x00 #x00 #x7e #x00 #x00 #x00 #x00
  #x00 #x00 #x00 #x00 #x00 #x18 #x18 #x00 #x02 #x06 #x0c #x18 #x30 #x60 #x40 #x00
  #x3c #x66 #x6e #x76 #x66 #x66 #x3c #x00 #x18 #x38 #x18 #x18 #x18 #x18 #x7e #x00
  #x3c #x66 #x06 #x0c #x18 #x30 #x7e #x00 #x7e #x0c #x18 #x0c #x06 #x66 #x3c #x00
  #x0c #x1c #x3c #x6c #x7e #x0c #x0c #x00 #x7e #x60 #x7c #x06 #x06 #x66 #x3c #x00
  #x3c #x60 #x60 #x7c #x66 #x66 #x3c #x00 #x7e #x06 #x0c #x18 #x30 #x30 #x30 #x00
  #x3c #x66 #x66 #x3c #x66 #x66 #x3c #x00 #x3c #x66 #x66 #x3e #x06 #x0c #x38 #x00
  #x00 #x60 #x60 #x00 #x60 #x60 #x00 #x00 #x00 #x30 #x30 #x00 #x30 #x30 #x60 #x00
  #x0c #x18 #x30 #x60 #x30 #x18 #x0c #x00 #x00 #x00 #x7e #x00 #x00 #x7e #x00 #x00
  #x60 #x30 #x18 #x0c #x18 #x30 #x60 #x00 #x3c #x66 #x06 #x0c #x18 #x00 #x18 #x00
  #x3c #x66 #x6e #x6a #x6e #x60 #x3e #x00 #x18 #x3c #x66 #x66 #x7e #x66 #x66 #x00
  #x7c #x66 #x66 #x7c #x66 #x66 #x7c #x00 #x3c #x66 #x60 #x60 #x60 #x66 #x3c #x00
  #x78 #x6c #x66 #x66 #x66 #x6c #x78 #x00 #x7e #x60 #x60 #x7c #x60 #x60 #x7e #x00
  #x7e #x60 #x60 #x7c #x60 #x60 #x60 #x00 #x3e #x60 #x60 #x6e #x66 #x66 #x3e #x00
  #x66 #x66 #x66 #x7e #x66 #x66 #x66 #x00 #x78 #x30 #x30 #x30 #x30 #x30 #x78 #x00
  #x06 #x06 #x06 #x06 #x06 #x66 #x3c #x00 #x66 #x6c #x78 #x70 #x78 #x6c #x66 #x00
  #x60 #x60 #x60 #x60 #x60 #x60 #x7e #x00 #xc6 #xee #xfe #xd6 #xc6 #xc6 #xc6 #x00
  #x66 #x76 #x7e #x7e #x6e #x66 #x66 #x00 #x3c #x66 #x66 #x66 #x66 #x66 #x3c #x00
  #x7c #x66 #x66 #x7c #x60 #x60 #x60 #x00 #x3c #x66 #x66 #x66 #x76 #x6c #x36 #x00
  #x7c #x66 #x66 #x7c #x6c #x66 #x66 #x00 #x3c #x66 #x60 #x3c #x06 #x66 #x3c #x00
  #x7e #x18 #x18 #x18 #x18 #x18 #x18 #x00 #x66 #x66 #x66 #x66 #x66 #x66 #x3e #x00
  #x66 #x66 #x66 #x66 #x66 #x3c #x18 #x00 #xc6 #xc6 #xc6 #xd6 #xfe #xee #xc6 #x00
  #x66 #x66 #x3c #x18 #x3c #x66 #x66 #x00 #x66 #x66 #x66 #x3c #x18 #x18 #x18 #x00
  #x7e #x06 #x0c #x18 #x30 #x60 #x7e #x00 #x78 #x60 #x60 #x60 #x60 #x60 #x78 #x00
  #x40 #x60 #x30 #x18 #x0c #x06 #x02 #x00 #x78 #x18 #x18 #x18 #x18 #x18 #x78 #x00
  #x10 #x38 #x6c #xc6 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xfe #x00
  #x00 #xc0 #x60 #x30 #x00 #x00 #x00 #x00 #x00 #x00 #x3c #x06 #x3e #x66 #x3e #x00
  #x60 #x60 #x7c #x66 #x66 #x66 #x7c #x00 #x00 #x00 #x3c #x60 #x60 #x60 #x3c #x00
  #x06 #x06 #x3e #x66 #x66 #x66 #x3e #x00 #x00 #x00 #x3c #x66 #x7e #x60 #x3c #x00
  #x1c #x30 #x7c #x30 #x30 #x30 #x30 #x00 #x00 #x00 #x3e #x66 #x66 #x3e #x06 #x7c
  #x60 #x60 #x7c #x66 #x66 #x66 #x66 #x00 #x30 #x00 #x70 #x30 #x30 #x30 #x78 #x00
  #x18 #x00 #x18 #x18 #x18 #x18 #x18 #x70 #x60 #x60 #x66 #x6c #x78 #x6c #x66 #x00
  #x70 #x30 #x30 #x30 #x30 #x30 #x78 #x00 #x00 #x00 #xec #xfe #xd6 #xc6 #xc6 #x00
  #x00 #x00 #x7c #x66 #x66 #x66 #x66 #x00 #x00 #x00 #x3c #x66 #x66 #x66 #x3c #x00
  #x00 #x00 #x7c #x66 #x66 #x66 #x7c #x60 #x00 #x00 #x3e #x66 #x66 #x66 #x3e #x06
  #x00 #x00 #x7c #x66 #x60 #x60 #x60 #x00 #x00 #x00 #x3e #x60 #x3c #x06 #x7c #x00
  #x00 #x18 #x7e #x18 #x18 #x18 #x0e #x00 #x00 #x00 #x66 #x66 #x66 #x66 #x3e #x00
  #x00 #x00 #x66 #x66 #x66 #x3c #x18 #x00 #x00 #x00 #xc6 #xc6 #xd6 #x7c #x6c #x00
  #x00 #x00 #x66 #x3c #x18 #x3c #x66 #x00 #x00 #x00 #x66 #x66 #x66 #x3e #x06 #x7c
  #x00 #x00 #x7e #x0c #x18 #x30 #x7e #x00 #x1c #x30 #x30 #x60 #x30 #x30 #x1c #x00
  #x60 #x60 #x60 #x60 #x60 #x60 #x60 #x60 #x70 #x18 #x18 #x0c #x18 #x18 #x70 #x00
  #x00 #x60 #xf2 #x9e #x0c #x00 #x00 #x00 #x00 #x18 #x18 #x34 #x34 #x62 #x7e #x00
  #x00 #x3c #x66 #x60 #x66 #x3c #x08 #x38 #x66 #x00 #x00 #x66 #x66 #x66 #x3e #x00
  #x0c #x18 #x00 #x3c #x7e #x60 #x3c #x00 #x18 #x66 #x00 #x3c #x06 #x7e #x3e #x00
  #x66 #x00 #x3c #x06 #x3e #x66 #x3e #x00 #x30 #x18 #x00 #x3c #x06 #x7e #x3e #x00
  #x18 #x18 #x00 #x3c #x06 #x7e #x3e #x00 #x00 #x00 #x3c #x60 #x60 #x3c #x08 #x18
  #x18 #x66 #x00 #x3c #x7e #x60 #x3c #x00 #x66 #x00 #x3c #x66 #x7e #x60 #x3c #x00
  #x30 #x18 #x00 #x3c #x7e #x60 #x3c #x00 #x66 #x00 #x00 #x38 #x18 #x18 #x3c #x00
  #x18 #x66 #x00 #x38 #x18 #x18 #x3c #x00 #x60 #x30 #x00 #x38 #x18 #x18 #x3c #x00
  #x66 #x00 #x18 #x3c #x66 #x7e #x66 #x00 #x18 #x00 #x18 #x3c #x66 #x7e #x66 #x00
  #x0c #x18 #x7e #x60 #x7c #x60 #x7e #x00 #x00 #x00 #x7e #x1b #x7f #xd8 #x7e #x00
  #x3f #x78 #xd8 #xde #xf8 #xd8 #xdf #x00 #x18 #x66 #x00 #x3c #x66 #x66 #x3c #x00
  #x66 #x00 #x00 #x3c #x66 #x66 #x3c #x00 #x30 #x18 #x00 #x3c #x66 #x66 #x3c #x00
  #x18 #x66 #x00 #x66 #x66 #x66 #x3e #x00 #x30 #x18 #x00 #x66 #x66 #x66 #x3e #x00
  #x66 #x00 #x66 #x66 #x66 #x3e #x06 #x7c #x66 #x00 #x3c #x66 #x66 #x66 #x3c #x00
  #x66 #x00 #x66 #x66 #x66 #x66 #x3e #x00 #x18 #x18 #x3c #x60 #x60 #x3c #x18 #x18
  #x1c #x3a #x30 #x7c #x30 #x30 #x7e #x00 #x66 #x66 #x3c #x18 #x3c #x18 #x18 #x00
  #x1c #x36 #x66 #x7c #x66 #x66 #x7c #x60 #x1e #x30 #x7c #x30 #x30 #x30 #x60 #x00
  #x0c #x18 #x00 #x3c #x06 #x7e #x3e #x00 #x0c #x18 #x00 #x38 #x18 #x18 #x3c #x00
  #x0c #x18 #x00 #x3c #x66 #x66 #x3c #x00 #x0c #x18 #x00 #x66 #x66 #x66 #x3e #x00
  #x34 #x58 #x00 #x7c #x66 #x66 #x66 #x00 #x34 #x58 #x00 #x66 #x76 #x6e #x66 #x00
  #x00 #x3c #x06 #x3e #x66 #x3e #x00 #x3c #x00 #x3c #x66 #x66 #x66 #x3c #x00 #x3c
  #x00 #x18 #x00 #x18 #x30 #x60 #x66 #x3c #x00 #x00 #x00 #x3e #x30 #x30 #x30 #x00
  #x00 #x00 #x00 #x7c #x0c #x0c #x0c #x00 #xc6 #xcc #xd8 #x36 #x6b #xc3 #x86 #x0f
  #xc6 #xcc #xd8 #x36 #x6e #xd6 #x9f #x06 #x00 #x18 #x00 #x18 #x18 #x18 #x18 #x18
  #x1b #x36 #x6c #xd8 #x6c #x36 #x1b #x00 #xd8 #x6c #x36 #x1b #x36 #x6c #xd8 #x00
  #x34 #x58 #x00 #x3c #x06 #x7e #x3e #x00 #x34 #x58 #x00 #x3c #x66 #x66 #x3c #x00
  #x02 #x3c #x66 #x6e #x76 #x66 #x3c #x40 #x00 #x02 #x3c #x6e #x76 #x66 #x3c #x40
  #x00 #x00 #x7e #xdb #xdf #xd8 #x7e #x00 #x7f #xd8 #xd8 #xde #xd8 #xd8 #x7f #x00
  #x30 #x18 #x00 #x18 #x3c #x66 #x7e #x66 #x34 #x58 #x00 #x18 #x3c #x66 #x7e #x66
  #x34 #x58 #x3c #x66 #x66 #x66 #x66 #x3c #x66 #x00 #x00 #x00 #x00 #x00 #x00 #x00
  #x18 #x30 #x60 #x00 #x00 #x00 #x00 #x00 #x00 #x20 #x70 #x20 #x20 #x20 #x00 #x00
  #x7a #xca #xca #xca #x7a #x0a #x0a #x0a #x7e #xc3 #xbd #xb1 #xb1 #xbd #xc3 #x7e
  #x7e #xc3 #xbd #xa5 #xb9 #xad #xc3 #x7e #xf1 #x5b #x5f #x55 #x51 #x00 #x00 #x00
  #x66 #x00 #xe6 #x66 #x66 #xf6 #x06 #x1c #xf6 #x66 #x66 #x66 #x66 #xf6 #x06 #x1c
  #x00 #x66 #x76 #x3c #x6e #x66 #x00 #x00 #x00 #x7c #x0c #x0c #x0c #x7e #x00 #x00
  #x00 #x1e #x06 #x0e #x1e #x36 #x00 #x00 #x00 #x7e #x0c #x0c #x0c #x0c #x00 #x00
  #x00 #x7c #x06 #x66 #x66 #x66 #x00 #x00 #x00 #x70 #x30 #x30 #x30 #x30 #x00 #x00
  #x00 #x78 #x30 #x18 #x18 #x18 #x00 #x00 #x00 #x7e #x36 #x36 #x36 #x36 #x00 #x00
  #x60 #x6e #x66 #x66 #x66 #x7e #x00 #x00 #x00 #x78 #x18 #x18 #x00 #x00 #x00 #x00
  #x00 #x7c #x0c #x0c #x0c #x7c #x00 #x00 #x60 #x7e #x06 #x06 #x06 #x0e #x00 #x00
  #x00 #x6c #x3e #x66 #x66 #x6e #x00 #x00 #x00 #x38 #x18 #x18 #x18 #x78 #x00 #x00
  #x00 #x7c #x6c #x6c #x6c #x38 #x00 #x00 #x00 #x36 #x36 #x36 #x36 #x7e #x00 #x00
  #x00 #x7e #x66 #x76 #x06 #x7e #x00 #x00 #x00 #x66 #x66 #x3c #x0e #x7e #x00 #x00
  #x00 #x7c #x0c #x6c #x6c #x68 #x60 #x00 #x00 #x78 #x0c #x0c #x0c #x0c #x00 #x00
  #x00 #xd6 #xd6 #xd6 #xd6 #xfe #x00 #x00 #x00 #x7c #x6c #x6c #x6c #xec #x00 #x00
  #x00 #x70 #x30 #x30 #x30 #x30 #x30 #x00 #x00 #x7c #x0c #x0c #x0c #x0c #x0c #x00
  #x00 #xfe #x66 #x66 #x66 #x7e #x00 #x00 #x00 #x7e #x66 #x76 #x06 #x06 #x06 #x00
  #x00 #x6c #x6c #x38 #x18 #x18 #x18 #x00 #x0e #x1b #x3c #x66 #x66 #x3c #xd8 #x70
  #x00 #x10 #x38 #x6c #xc6 #x82 #x00 #x00 #x66 #xf7 #x99 #x99 #xef #x66 #x00 #x00
  #x00 #x00 #x76 #xdc #xc8 #xdc #x76 #x00 #x1c #x36 #x66 #x7c #x66 #x66 #x7c #x60
  #x00 #xfe #x66 #x62 #x60 #x60 #x60 #xf8 #x00 #x00 #xfe #x6c #x6c #x6c #x6c #x48
  #xfe #x66 #x30 #x18 #x30 #x66 #xfe #x00 #x00 #x1e #x38 #x6c #x6c #x6c #x38 #x00
  #x00 #x00 #x6c #x6c #x6c #x6c #x7f #xc0 #x00 #x00 #x7e #x18 #x18 #x18 #x18 #x10
  #x3c #x18 #x3c #x66 #x66 #x3c #x18 #x3c #x00 #x3c #x66 #x7e #x66 #x66 #x3c #x00
  #x00 #x3c #x66 #x66 #x66 #x24 #x66 #x00 #x1c #x36 #x78 #xdc #xcc #xec #x78 #x00
  #x0c #x18 #x38 #x54 #x54 #x38 #x30 #x60 #x00 #x10 #x7c #xd6 #xd6 #xd6 #x7c #x10
  #x3e #x70 #x60 #x7e #x60 #x70 #x3e #x00 #x3c #x66 #x66 #x66 #x66 #x66 #x66 #x00
  #x00 #x7e #x00 #x7e #x00 #x7e #x00 #x00 #x18 #x18 #x7e #x18 #x18 #x00 #x7e #x00
  #x30 #x18 #x0c #x18 #x30 #x00 #x7e #x00 #x0c #x18 #x30 #x18 #x0c #x00 #x7e #x00
  #x00 #x0e #x1b #x1b #x18 #x18 #x18 #x18 #x18 #x18 #x18 #x18 #xd8 #xd8 #x70 #x00
  #x18 #x18 #x00 #x7e #x00 #x18 #x18 #x00 #x00 #x32 #x4c #x00 #x32 #x4c #x00 #x00
  #x38 #x6c #x38 #x00 #x00 #x00 #x00 #x00 #x38 #x7c #x38 #x00 #x00 #x00 #x00 #x00
  #x00 #x00 #x00 #x00 #x60 #x60 #x00 #x00 #x00 #x00 #x0f #x18 #xd8 #x70 #x30 #x00
  #x38 #x6c #x6c #x6c #x6c #x00 #x00 #x00 #x38 #x6c #x18 #x30 #x7c #x00 #x00 #x00
  #x78 #x0c #x38 #x0c #x78 #x00 #x00 #x00 #x00 #xfe #x00 #x00 #x00 #x00 #x00 #x00)

(include! "nienor/lib/malloc.scm")
