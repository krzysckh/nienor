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

(alloc! mouse 0 0)

(nalloc! *state* 4096)

(define-vector (draw-handler)
  (fill! 0 0 color-3 layer-0)
  (set8! mouse-sprite (+ 1 (get8! mouse-sprite)))
  (sprite! (mouse-x) (mouse-y) mouse-sprite 0 0 0 0 color-1)

  (putchar #\!)

  (let ((mx (mouse-x))
        (my (mouse-y)))
    (loopn (i 0 64 1)
      (loopn (j 0 64 1)
        (set8! (+ *state* (+ (* (/ my 10) 64) (/ mx 10))) 1)
         (when (equ? 1 (get8! (+ *state* (+ (* 64 i) j))))
           (sprite! (* j 10) (* i 10) smiley-sprite 0 0 0 0 color-1))))))

(define (main)
  (set-screen-width! 640)
  (set-screen-height! 640)
  (set-colors! #xff00 #xff00 #xff00)
  (set-mouse-handler! mouse-handler)
  (set-draw-handler! draw-handler))

(define-vector (mouse-handler)
  (set! mouse (mouse-state)))
