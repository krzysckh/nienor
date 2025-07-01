(define w 400)
(define h 300)

(defvar state)

(alloc! spr
  #b10010010
  #b00100100
  #b01001001
  #b10010010
  #b00100100
  #b01001001
  #b10010010
  #b00100100)

;; Yes, i know this is pointless. It's just to test out symbols

(define-vector (mouse)
  (let ((x (mouse-x)))
    (set! state (cond
                 ((<= x 100) 'part-1)
                 ((<= x 200) 'part-2)
                 ((<= x 300) 'part-3)
                 (else       'part-4)))))

(define-vector (draw)
  (fill! 0 0 color-1 layer-1)
  (let ((x (cond
            ((equ? state 'part-1) 0)
            ((equ? state 'part-2) 100)
            ((equ? state 'part-3) 200)
            ((equ? state 'part-4) 300))))
    (fill-rect! spr color-2 x 0 (+ x 100) h layer-1)))

(define (main)
  (set-colors! #x0f00 0 0)
  (set-screen-size! w h)
  (set-mouse-handler! mouse)
  (set-draw-handler! draw))
