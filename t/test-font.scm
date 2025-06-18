(alloc! *texture*
  #b10010010
  #b00100100
  #b01001001
  #b10010010
  #b00100100
  #b01001001
  #b10010010
  #b00100100)

(defvar *hello* "hello world" 0)
(define text-width 88)
(define screen-width 400)
(define screen-height 300)
(define vx 4)
(define vy 4)

;; TODO: fix this syntax for small shorts
(defvar x   0 10)
(defvar y   0 10)
(defvar mx  0 1) ; mode-{x,y}: %2 == 1 for +, 0 for -
(defvar my  0 1)
(defvar update!)

(define-vector (draw)
  (let ((ax (- x (modulo x 8)))
        (ay (- y (modulo y 8))))
    (fill-rect! *texture* color-2 ax ay (+ ax (+ 1 text-width)) (+ ay 16) layer-1)) ; patch up old hello world
  (when (bior (band (modulo mx 2) (> (+ x text-width) screen-width)) (> x screen-width))
    (set! mx (+ mx 1)))
  (when (bior (band (modulo my 2) (> (+ y 8) screen-height)) (> y screen-height))
    (set! my (+ my 1)))

  (if (modulo mx 2)
      (set! x (+ x vx))
      (set! x (- x vx)))
  (if (modulo my 2)
      (set! y (+ y vy))
      (set! y (- y vy)))

  (vputs (addrof *hello*) x y #b00001010 layer-1))

(define (main)
  (set-colors! #x05ff #x00ff #x0dff)
  (set-draw-handler! draw)
  (set-screen-width! screen-width)
  (set-screen-height! screen-height)
  (fill-rect! *texture* color-2 0 0 400 300 layer-0))
