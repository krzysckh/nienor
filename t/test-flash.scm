;; "6 colors"

(define r1 #x0f00)
(define g1 #x00f0)
(define b1 #x000f)

(define r2 #x0f0f)
(define g2 #x0ff0)
(define b2 #x00ff)

(define rect-width 24)
(define rect-height 48)

(alloc! spr #xff #xff #xff #xff #xff #xff #xff #xff)

(define-vector (a)
  (set-colors! r1 g1 b1)
  (fill! 0 0 0 0)
  (fill-rect spr color-2 (* rect-width 1) 0 (* rect-width 2) rect-height 0)
  (fill-rect spr color-3 (* rect-width 3) 0 (* rect-width 4) rect-height 0)
  (fill-rect spr color-4 (* rect-width 5) 0 (* rect-width 6) rect-height 0)
  (set-draw-handler! b))

(define-vector (b)
  (set-colors! r2 g2 b2)
  (fill! 0 0 0 0)
  (fill-rect spr color-2 (* rect-width 2) 0 (* rect-width 3) rect-height 0)
  (fill-rect spr color-3 (* rect-width 4) 0 (* rect-width 5) rect-height 0)
  (fill-rect spr color-4 (* rect-width 6) 0 (* rect-width 7) rect-height 0)
  (set-draw-handler! a))

(define (main)
  (set-screen-width! (* rect-width 7))
  (set-screen-height! rect-height)
  (set-draw-handler! a))
