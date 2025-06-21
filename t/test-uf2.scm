(alloc! fnt-name "/home/kpm/nmojeprogramy/uxn/projects/fonts/sans14-regular.uf2" 0)
(alloc! text "The quick brown fox jumps over the lazy dog." 0)

(defvar fnt)

(define (main)
  (set-colors! #x0ff0 #x0f00 #x0f0f)
  (set-draw-handler! draw)
  (set! fnt (uf2/load fnt-name)))

(define-vector (draw)
  (uf2/vputs fnt 10 10 text layer-0 color-2))
