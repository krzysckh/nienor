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

(alloc!
 smiley-sprite
 #b00000000
 #b01000010
 #b01000010
 #b00000000
 #b10000001
 #b01000010
 #b00111100
 #b00000000)

(defvar mouse 0)
(defvar *state*)

(define-vector (draw-handler)
  (fill! 0 0 color-4 layer-0)
  (set8! mouse-sprite (+ 1 (get8! mouse-sprite)))

  (let ((mx (mouse-x))
        (my (mouse-y)))
    (sprite! mx my mouse-sprite 0 0 0 0 color-1)

    (loopn (i 0 64 1)
      (loopn (j 0 64 1)
        (when (mouse-state)
          (set8! (+ *state* (+ (* (/ my 10) 64) (/ mx 10))) 1))
        (when (get8! (+ *state* (+ (* 64 i) j)))
          (sprite! (* j 10) (* i 10) smiley-sprite 0 0 0 0 color-1))))))

(define (main)
  (set-screen-width! 640)
  (set-screen-height! 640)
  (set-colors! #xfff0 #xff00 #xff00)
  (set-mouse-handler! mouse-handler)
  (set-draw-handler! draw-handler)
  (set! *state* (malloc 4096))
  (print-number *state*)) ; (20/06/2025) lol *state* to 2137

(define-vector (mouse-handler)
  (pixel! (mouse-x) (mouse-y) color-3 0 layer-1 0 0))
