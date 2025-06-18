(define *width* 400)
(define *height* 300)

(alloc! *heart-texture*
  #b01000010
  #b11100111
  #b11111111
  #b11111111
  #b01111110
  #b00111100
  #b00011000
  #b00000000)

(defvar heart!)

(define (make-drawer sprite)
  (λ (x y)
    (sprite! x y sprite 0 layer-1 0 0 1)))

(define (main)
  (set-draw-handler! draw)
  (set-colors! #x0f00 #x00f0 #x000f)
  (set-screen-width! *width*)
  (set-screen-height! *height*)

  (set! heart! (make-drawer *heart-texture*)))

(define-vector (draw)
  (let ((x (modulo (rand) *width*))
        (y (modulo (rand) *height*)))
    (heart! x y)))
