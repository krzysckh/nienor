(alloc! *texture*
  #b10010010
  #b00100100
  #b01001001
  #b10010010
  #b00100100
  #b01001001
  #b10010010
  #b00100100)

(define text-width 88)
(define screen-width 400)
(define screen-height 300)

;; TODO: fix this syntax for small shorts
(defvar x  0 10)
(defvar y  0 10)
(defvar vx 0 4)
(defvar vy 0 4)

(define-vector (draw)
  (let ((ax (- x (modulo x 8)))
        (ay (- y (modulo y 8))))
    (fill-rect! *texture* color-2 ax ay (+ ax (+ 1 text-width)) (+ ay 16) layer-1) ; patch up old hello world

    (when (bior (> (+ x text-width) screen-width) (band (negative? vx) (equ? x 0)))
      (set! vx (signed* vx (negative! 1))))

    (when (bior (> (+ y 8) screen-height) (band (negative? vy) (equ? y 0)))
      (set! vy (signed* vy (negative! 1))))

    (set! x (signed-max2 0 (signed+ x vx)))
    (set! y (signed-max2 0 (signed+ y vy)))

    (vputs "hello world" x y #b00001010 layer-1)))

(define (main)
  (set-colors! #x05ff #x00ff #x0dff)
  (set-draw-handler! draw)
  (set-screen-size! screen-width screen-height)
  (fill-rect! *texture* color-2 0 0 400 300 layer-0))
