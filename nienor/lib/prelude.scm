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
  (flatten!
   (alloc! name . vs)
   (_declare-var! name)))

(define-macro-rule ()
  (defvar name)
  (defvar name 0 0))

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

   (define-macro-rule (_)
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

(define-label! ___alloc-arg)
  (uxn-call! (2) swp) ; we jumped here with JSR2k, so the address is kept, and we want to push the arg, not the address
  (pus! 0)
  (uxn-call! () ldz)  ; load ptr
  (uxn-call! () inc)  ; inc ptr
  (pus! 2)
  (uxn-call! () mul)  ; *2 because all pointers are shorts
  (uxn-call! (2) stz) ; store arg at ptr
  (pus! 0)
  (uxn-call! () ldz)
  (uxn-call! () inc) ; load & inc ptr again
  (pus! 0)
  (uxn-call! () stz) ; store new ptr at 0x0
  (uxn-call! (2 r) jmp)


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
  (bnot x)
  (if x #f #t))

(define-macro-rule ()
  (not x)
  (bnot x))

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

  (with-locals! (mask)
    (short->byte mask)
    (pus! #x2e)
    (deo!)))

(define (fill! x y color layer)
  (pixel! x y color fill-mode layer 0 0))

(define (draw-pixel! x y color)
  (pixel! x y color 0 0 0 0))

(define-simple-deo2 pick-sprite! #x2c)

(define (sprite! x y sprite bpp2? layer fx fy color)
  (pick-pixel! x y)
  (pick-sprite! sprite)

  (bior* bpp2? layer fx fy color)

  (with-locals! (mask)
    (short->byte mask)
    (pus! #x2f)
    (deo!)))

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

(define-macro-rule ()
  (get! addr)
  (_get! addr))

(define-macro-rule ()
  (set! place value)
  (begin
    (push! value)
    (push! (addrof place))
    (uxn-call! (2) sta)))

(define-macro-rule ()
  (addrof var)
  (_addrof var))

(define-macro-rule ()
  (get8! place)
  (begin
    (get! place)
    (uxn-call! () pop)
    (pus! 0)
    (uxn-call! () swp)))

(define-macro-rule ()
  (set8! place value)
  (begin
    (pus! value)
    (push! (addrof place))
    (uxn-call! () sta)))

(define-macro-rule ()
  (jmp! to)
  (begin
    to (uxn-call! (2) jmp)))

(define-macro-rule ()
  (with-locals! names . body)
  (_with-locals! names body))

(define-macro-rule ()
  (loopn (it from to by) . body)
  (begin
    to from
    (with-label _loop
      (uxn-call! (2) dup)
      (with-locals! (it) . body)
      (+ by (begin)) ; from += by ; begin as i don't have any way to say "take from the top of the stack" lmao!
      (if (byte->short (uxn-call! (k 2) gth))
          (jmp! _loop)
          (begin
            (uxn-call! (2) pop)
            (uxn-call! (2) pop))))))

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
  (with-local k v . body)
  (begin
    v (with-locals! (k) . body)))

(define-macro-rule ()
  (let* ((key val) . rest) . body)
  (with-local key val (let* rest . body)))

(define-macro-rule ()
  (let* ((key val)) . body)
  (with-local key val . body))

(define-macro-rule (_)
  (let ((key val) . rest) . body)
  (let (_ key) (_ val) rest . body))

(define-macro-rule (_)
  (let (_ . keys) (_ . values) ((key val) . rest) . body)
  (let (_ key . keys) (_ val . values) rest . body))

(define-macro-rule (_ ())
  (let (_ . keys) (_ . values) () . body)
  (begin
    (nigeb . values)
    (with-locals! (_reverse . keys) . body)))

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
  (let* ((x (get! *rand-seed-state*))
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

(define-macro-rule ()
  (min2 a b)
  (if (< a b) a b))

(define-macro-rule ()
  (max2 a b)
  (if (> a b) a b))

(include! "nienor/lib/malloc.scm")
(include! "nienor/lib/signed.scm")
(include! "nienor/lib/font.scm")
