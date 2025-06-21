(alloc! fnt-name "/home/kpm/nmojeprogramy/uxn/projects/fonts/sans14-regular.uf2" 0)
(alloc! text "The quick brown fox jumps over the lazy dog." 0)
(nalloc! _ 512) ; <- buffer for the edited text. unsafe. lol.

(define pad 8)

(alloc! *texture*
  #b10010010
  #b00100100
  #b01001001
  #b10010010
  #b00100100
  #b01001001
  #b10010010
  #b00100100)

(defvar fnt)

(define (main)
  (set-colors! #x0ff0 #x0f00 #x0f0f)
  (set-draw-handler! draw)
  (set-key-handler! key)
  (set! fnt (uf2/load fnt-name)))

(define-vector (key)
  (let* ((p (+ text (strlen text)))
         (b (button-pressed)))
    (if (equ? b 8)
        (set8! (- p 1) 0)
        (begin
          (set8! p b)
          (set8! (+ p 1) 0)))))

(define-vector (draw)
  (fill! 0 0 color-1 layer-0)
  (fill! 0 0 color-1 layer-1)
  (call-with-lines
   (λ (s ctr)
     (let ((w (uf2/measure-text fnt s))
           (y (+ (* pad ctr) (* 16 ctr))))
       (fill-rect! *texture* color-3 0 y w (+ 16 y) layer-0)
       (uf2/vputs fnt 0 y s layer-1 color-2)))
   text))
