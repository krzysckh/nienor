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

(define-vector (draw-handler)
  (fill! 0 0 color-3 layer-0)
  (sprite! (mouse-x) (mouse-y) mouse-sprite 0 0 0 0 color-1))

(define (main)
  (set-colors! #xff00 #xff00 #xff00)
  (set-draw-handler! draw-handler))
