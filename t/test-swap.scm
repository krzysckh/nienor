(define r1 #x0f00)
(define g1 #x00f0)
(define b1 #x000f)

(define r2 #x000f)
(define g2 #x0f00)
(define b2 #x00f0)

(alloc! *color* 0 0)
(alloc! *face-mode* 0 0)
(alloc! *frame-ctr* 0 0)

(alloc! *face*
  #b00000000
  #b01000010
  #b01100011
  #b00000000
  #b10000001
  #b10000001
  #b01100110
  #b00011000)

(define (update-theme)
  (if (equ? (get! *color*) 0)
      (begin
        (set! *color* 1)
        (set-colors! r2 g2 b2))
      (begin
        (set! *color* 0)
        (set-colors! r1 g1 b1))))

(define (main)
  (set-draw-handler! refresh))

(define-vector (refresh)
  (fill! 0 0 color-1 0)
  (if (equ? (get! *frame-ctr*) 16)
      (begin
        (update-theme)
        (set! *frame-ctr* 0))
      (set! *frame-ctr* (+ (get! *frame-ctr*) 1)))

  (let ((color-bits (band #x0f (+ (get! *face-mode*) 1))))
    (sprite! (mouse-x) (mouse-y) *face* 0 layer-1 0 0 color-bits)
    (set! *face-mode* color-bits)))
