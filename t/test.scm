;; -*- Owl -*-

(alloc!
 mouse-sprite
 #b11000000
 #b11100000
 #b11110000
 #b11111000
 #b11110000
 #b00110000
 #b00011000
 #b00001000)

(define (debug!)
  (pus! 1)
  (pus! #x0e)
  (deo!))

(define (+ a b)
  (push! a) ; i manually push here for the uxn-call! primitive
  (push! b)
  (uxn-call! (2) add))

(define-macro-rule
  (bior* a b)
  (bior a b))

(define-macro-rule
  (bior* a b . c)
  (bior a (bior* b . c)))

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

(define (<<8 a b)
  (pus! a)

  (pus! b)
  (pus! 4)
  (uxn-call! () sft)

  (uxn-call! (2) sft))

(define (>> a b)
  (push! a)
  (band8 b #b00001111)
  (uxn-call! (2) sft))

(define (>>8 a b)
  (pus! a)
  (band8 b #b00001111)
  (uxn-call! () sft))

(define (- a b)
  (push! b)
  (push! a)
  (uxn-call! (2) sub))

(define (putchar c)
  (pus! c)
  (pus! #x18)
  (uxn-call! () deo))

(define (exit code)
  (pus! code)
  (pus! #x0f)
  (uxn-call! () deo))

(define (equ? a b)
  (push! a)
  (push! b)
  (uxn-call! (2) equ))

(define (eq? a b)
  (pus! a)
  (pus! b)
  (uxn-call! () equ))

(define (set-draw-handler! f)
  (push! f)
  (pus! #x20)
  (uxn-call! (2) deo))

(define (set-colors! r g b)
  (push! r)
  (pus! #x08)
  (deo2!)

  (push! g)
  (pus! #x0a)
  (deo2!)

  (push! b)
  (pus! #x0c)
  (deo2!))

(define (pick-x! x)
  (push! x)
  (pus! #x28)
  (deo2!))

(define (pick-y! y)
  (push! y)
  (pus! #x2a)
  (deo2!))

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
  (free-locals! 1)
  ;; (putchar #\newline)
  )

(define (fill! x y color layer)
  (pixel! x y color fill-mode layer 0 0))

(define (pick-sprite! sprite)
  (push! sprite)
  (pus! #x2c)
  (deo2!))

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

(define-vector (draw-handler)
  (fill! 0 0 color-3 layer-0)
  (sprite! (mouse-x) (mouse-y) mouse-sprite 0 0 0 0 color-1))
  ;; (pixel! (mouse-x) (mouse-y) color-1 0 0 0 0))

(define (main)
  (set-colors! #xff00 #xff00 #xff00)
  (set-draw-handler! draw-handler)
  )
