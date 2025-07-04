;; filename-ptr → font-ptr
(define (uf2/load filename-ptr)
  (read-file filename-ptr))

;; returns x'
(define (uf2/vputc font x y chr layer color)
  (let ((w (get8! (+ font chr)))
        (dat (+ font #xff (* chr 4 8))))
    (sprite! x y dat 0 layer 0 0 color)
    (sprite! x (+ y 8) (+ dat 8) 0 layer 0 0 color)
    (when (> w 8)
      (sprite! (+ x 8) y (+ dat (* 2 8)) 0 layer 0 0 color)
      (sprite! (+ x 8) (+ y 8) (+ dat (* 3 8)) 0 layer 0 0 color))
    (+ x w)))

(define (uf2/vputs font x y str layer color)
  (let ((chr (get8! str)))
    (when (not (equ? chr 0))
      (uf2/vputs
       font
       (uf2/vputc font x y chr layer color)
       y
       (+ str 1)
       layer color))))

(define (uf2/_measure-text font str w)
  (let ((chr (get8! str)))
    (if (equ? chr 0)
        w
        (uf2/_measure-text font (+ str 1) (+ w (get8! (+ font chr)))))))

(define-macro-rule ()
  (uf2/measure-text font str)
  (uf2/_measure-text font str 0))

;; TODO: add uf1 & uf3 routines. uf1 is done already but uses only builtin atari8
(define (vputc chr x y color layer)
  (sprite! x y (+ *atari8* (* 8 chr)) 0 layer 0 0 color))

(define (vputs ptr x y color layer)
  (let ((chr (get8! ptr)))
    (when (not (equ? chr 0))
      (vputc chr x y color layer)
      (vputs (+ ptr 1) (+ x 8) y color layer))))

(alloc! *atari8*
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x04 #x08 #x08 #x08 #x08 #x08 #x04 #x08 #x08 #x08 #x08 #x05 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x04 #x05 #x07 #x08 #x07 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x06 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x06 #x08 #x06 #x08 #x08
  #x05 #x08 #x08 #x07 #x08 #x08 #x07 #x08 #x08 #x06 #x06 #x08 #x06 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x07 #x04 #x07 #x07 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x07 #x08 #x08 #x08 #x08 #x08 #x07 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x06 #x05 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x05 #x06 #x08 #x08 #x06 #x07 #x08 #x08 #x06
  #x07 #x08 #x08 #x08 #x07 #x07 #x08 #x07 #x05 #x07 #x08 #x08 #x07 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08
  #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x08 #x07 #x04 #x08 #x07 #x07 #x07 #x07
  #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x18 #x3c #x66 #xc3 #xe7 #x24 #x24 #x3c
  #x3c #x24 #x24 #xe7 #xc3 #x66 #x3c #x18 #x18 #x1c #xf6 #x83 #x83 #xf6 #x1c #x18
  #x18 #x38 #x6f #xc1 #xc1 #x6f #x38 #x18 #x3c #x99 #xc3 #xe7 #xc3 #x99 #x3c #x00
  #xff #xff #xfe #xfc #xf9 #xf3 #xe7 #x00 #xe7 #xc3 #x99 #x3c #x99 #xc3 #xe7 #x00
  #x01 #x03 #x06 #x8c #xd8 #x70 #x20 #x00 #x7e #xc3 #xd3 #xd3 #xdb #xc3 #xc3 #x7e
  #x18 #x3c #x3c #x3c #x7e #x10 #x38 #x10 #x18 #x1c #x16 #x10 #x10 #x70 #xf0 #x60
  #xf0 #xc0 #xfe #xd8 #xde #x18 #x18 #x00 #xf0 #xc0 #xdf #xdb #xff #x1e #x1b #x00
  #x05 #x05 #x05 #x0d #x0d #x19 #x79 #x71 #xa0 #xa0 #xa0 #xb0 #xb0 #x98 #x9e #x8e
  #x7c #xc6 #xc6 #x00 #xc6 #xc6 #x7c #x00 #x06 #x06 #x06 #x00 #x06 #x06 #x06 #x00
  #x7c #x06 #x06 #x7c #xc0 #xc0 #x7c #x00 #x7c #x06 #x06 #x7c #x06 #x06 #x7c #x00
  #xc6 #xc6 #xc6 #x7c #x06 #x06 #x06 #x00 #x7c #xc0 #xc0 #x7c #x06 #x06 #x7c #x00
  #x7c #xc0 #xc0 #x7c #xc6 #xc6 #x7c #x00 #x7c #x06 #x06 #x00 #x06 #x06 #x06 #x00
  #x7c #xc6 #xc6 #x7c #xc6 #xc6 #x7c #x00 #x7c #xc6 #xc6 #x7c #x06 #x06 #x7c #x00
  #x00 #x00 #x3c #x06 #x7e #x66 #x3c #x00 #x78 #x60 #x78 #x60 #x7e #x18 #x1e #x00
  #x07 #x0f #x1f #x18 #x18 #x10 #x1e #x17 #xf0 #xf8 #xec #x04 #x04 #x04 #x3c #x54
  #x11 #x0b #x0d #x06 #x07 #x2e #x39 #x38 #x04 #x28 #xd8 #x28 #xd0 #x10 #xe0 #x00
  #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x60 #x60 #x60 #x60 #x60 #x00 #x60 #x00
  #x66 #x66 #x66 #x00 #x00 #x00 #x00 #x00 #x00 #x6c #xfe #x6c #x6c #xfe #x6c #x00
  #x18 #x3e #x60 #x3c #x06 #x7c #x18 #x00 #x00 #x66 #x6c #x18 #x30 #x66 #x46 #x00
  #x38 #x6c #x38 #x70 #xde #xcc #x76 #x00 #x60 #x60 #x60 #x00 #x00 #x00 #x00 #x00
  #x0e #x1c #x18 #x18 #x18 #x1c #x0e #x00 #x70 #x38 #x18 #x18 #x18 #x38 #x70 #x00
  #x00 #x66 #x3c #xff #x3c #x66 #x00 #x00 #x00 #x18 #x18 #x7e #x18 #x18 #x00 #x00
  #x00 #x00 #x00 #x00 #x00 #x30 #x30 #x60 #x00 #x00 #x00 #x7e #x00 #x00 #x00 #x00
  #x00 #x00 #x00 #x00 #x00 #x18 #x18 #x00 #x02 #x06 #x0c #x18 #x30 #x60 #x40 #x00
  #x3c #x66 #x6e #x76 #x66 #x66 #x3c #x00 #x18 #x38 #x18 #x18 #x18 #x18 #x7e #x00
  #x3c #x66 #x06 #x0c #x18 #x30 #x7e #x00 #x7e #x0c #x18 #x0c #x06 #x66 #x3c #x00
  #x0c #x1c #x3c #x6c #x7e #x0c #x0c #x00 #x7e #x60 #x7c #x06 #x06 #x66 #x3c #x00
  #x3c #x60 #x60 #x7c #x66 #x66 #x3c #x00 #x7e #x06 #x0c #x18 #x30 #x30 #x30 #x00
  #x3c #x66 #x66 #x3c #x66 #x66 #x3c #x00 #x3c #x66 #x66 #x3e #x06 #x0c #x38 #x00
  #x00 #x60 #x60 #x00 #x60 #x60 #x00 #x00 #x00 #x30 #x30 #x00 #x30 #x30 #x60 #x00
  #x0c #x18 #x30 #x60 #x30 #x18 #x0c #x00 #x00 #x00 #x7e #x00 #x00 #x7e #x00 #x00
  #x60 #x30 #x18 #x0c #x18 #x30 #x60 #x00 #x3c #x66 #x06 #x0c #x18 #x00 #x18 #x00
  #x3c #x66 #x6e #x6a #x6e #x60 #x3e #x00 #x18 #x3c #x66 #x66 #x7e #x66 #x66 #x00
  #x7c #x66 #x66 #x7c #x66 #x66 #x7c #x00 #x3c #x66 #x60 #x60 #x60 #x66 #x3c #x00
  #x78 #x6c #x66 #x66 #x66 #x6c #x78 #x00 #x7e #x60 #x60 #x7c #x60 #x60 #x7e #x00
  #x7e #x60 #x60 #x7c #x60 #x60 #x60 #x00 #x3e #x60 #x60 #x6e #x66 #x66 #x3e #x00
  #x66 #x66 #x66 #x7e #x66 #x66 #x66 #x00 #x78 #x30 #x30 #x30 #x30 #x30 #x78 #x00
  #x06 #x06 #x06 #x06 #x06 #x66 #x3c #x00 #x66 #x6c #x78 #x70 #x78 #x6c #x66 #x00
  #x60 #x60 #x60 #x60 #x60 #x60 #x7e #x00 #xc6 #xee #xfe #xd6 #xc6 #xc6 #xc6 #x00
  #x66 #x76 #x7e #x7e #x6e #x66 #x66 #x00 #x3c #x66 #x66 #x66 #x66 #x66 #x3c #x00
  #x7c #x66 #x66 #x7c #x60 #x60 #x60 #x00 #x3c #x66 #x66 #x66 #x76 #x6c #x36 #x00
  #x7c #x66 #x66 #x7c #x6c #x66 #x66 #x00 #x3c #x66 #x60 #x3c #x06 #x66 #x3c #x00
  #x7e #x18 #x18 #x18 #x18 #x18 #x18 #x00 #x66 #x66 #x66 #x66 #x66 #x66 #x3e #x00
  #x66 #x66 #x66 #x66 #x66 #x3c #x18 #x00 #xc6 #xc6 #xc6 #xd6 #xfe #xee #xc6 #x00
  #x66 #x66 #x3c #x18 #x3c #x66 #x66 #x00 #x66 #x66 #x66 #x3c #x18 #x18 #x18 #x00
  #x7e #x06 #x0c #x18 #x30 #x60 #x7e #x00 #x78 #x60 #x60 #x60 #x60 #x60 #x78 #x00
  #x40 #x60 #x30 #x18 #x0c #x06 #x02 #x00 #x78 #x18 #x18 #x18 #x18 #x18 #x78 #x00
  #x10 #x38 #x6c #xc6 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #xfe #x00
  #x00 #xc0 #x60 #x30 #x00 #x00 #x00 #x00 #x00 #x00 #x3c #x06 #x3e #x66 #x3e #x00
  #x60 #x60 #x7c #x66 #x66 #x66 #x7c #x00 #x00 #x00 #x3c #x60 #x60 #x60 #x3c #x00
  #x06 #x06 #x3e #x66 #x66 #x66 #x3e #x00 #x00 #x00 #x3c #x66 #x7e #x60 #x3c #x00
  #x1c #x30 #x7c #x30 #x30 #x30 #x30 #x00 #x00 #x00 #x3e #x66 #x66 #x3e #x06 #x7c
  #x60 #x60 #x7c #x66 #x66 #x66 #x66 #x00 #x30 #x00 #x70 #x30 #x30 #x30 #x78 #x00
  #x18 #x00 #x18 #x18 #x18 #x18 #x18 #x70 #x60 #x60 #x66 #x6c #x78 #x6c #x66 #x00
  #x70 #x30 #x30 #x30 #x30 #x30 #x78 #x00 #x00 #x00 #xec #xfe #xd6 #xc6 #xc6 #x00
  #x00 #x00 #x7c #x66 #x66 #x66 #x66 #x00 #x00 #x00 #x3c #x66 #x66 #x66 #x3c #x00
  #x00 #x00 #x7c #x66 #x66 #x66 #x7c #x60 #x00 #x00 #x3e #x66 #x66 #x66 #x3e #x06
  #x00 #x00 #x7c #x66 #x60 #x60 #x60 #x00 #x00 #x00 #x3e #x60 #x3c #x06 #x7c #x00
  #x00 #x18 #x7e #x18 #x18 #x18 #x0e #x00 #x00 #x00 #x66 #x66 #x66 #x66 #x3e #x00
  #x00 #x00 #x66 #x66 #x66 #x3c #x18 #x00 #x00 #x00 #xc6 #xc6 #xd6 #x7c #x6c #x00
  #x00 #x00 #x66 #x3c #x18 #x3c #x66 #x00 #x00 #x00 #x66 #x66 #x66 #x3e #x06 #x7c
  #x00 #x00 #x7e #x0c #x18 #x30 #x7e #x00 #x1c #x30 #x30 #x60 #x30 #x30 #x1c #x00
  #x60 #x60 #x60 #x60 #x60 #x60 #x60 #x60 #x70 #x18 #x18 #x0c #x18 #x18 #x70 #x00
  #x00 #x60 #xf2 #x9e #x0c #x00 #x00 #x00 #x00 #x18 #x18 #x34 #x34 #x62 #x7e #x00
  #x00 #x3c #x66 #x60 #x66 #x3c #x08 #x38 #x66 #x00 #x00 #x66 #x66 #x66 #x3e #x00
  #x0c #x18 #x00 #x3c #x7e #x60 #x3c #x00 #x18 #x66 #x00 #x3c #x06 #x7e #x3e #x00
  #x66 #x00 #x3c #x06 #x3e #x66 #x3e #x00 #x30 #x18 #x00 #x3c #x06 #x7e #x3e #x00
  #x18 #x18 #x00 #x3c #x06 #x7e #x3e #x00 #x00 #x00 #x3c #x60 #x60 #x3c #x08 #x18
  #x18 #x66 #x00 #x3c #x7e #x60 #x3c #x00 #x66 #x00 #x3c #x66 #x7e #x60 #x3c #x00
  #x30 #x18 #x00 #x3c #x7e #x60 #x3c #x00 #x66 #x00 #x00 #x38 #x18 #x18 #x3c #x00
  #x18 #x66 #x00 #x38 #x18 #x18 #x3c #x00 #x60 #x30 #x00 #x38 #x18 #x18 #x3c #x00
  #x66 #x00 #x18 #x3c #x66 #x7e #x66 #x00 #x18 #x00 #x18 #x3c #x66 #x7e #x66 #x00
  #x0c #x18 #x7e #x60 #x7c #x60 #x7e #x00 #x00 #x00 #x7e #x1b #x7f #xd8 #x7e #x00
  #x3f #x78 #xd8 #xde #xf8 #xd8 #xdf #x00 #x18 #x66 #x00 #x3c #x66 #x66 #x3c #x00
  #x66 #x00 #x00 #x3c #x66 #x66 #x3c #x00 #x30 #x18 #x00 #x3c #x66 #x66 #x3c #x00
  #x18 #x66 #x00 #x66 #x66 #x66 #x3e #x00 #x30 #x18 #x00 #x66 #x66 #x66 #x3e #x00
  #x66 #x00 #x66 #x66 #x66 #x3e #x06 #x7c #x66 #x00 #x3c #x66 #x66 #x66 #x3c #x00
  #x66 #x00 #x66 #x66 #x66 #x66 #x3e #x00 #x18 #x18 #x3c #x60 #x60 #x3c #x18 #x18
  #x1c #x3a #x30 #x7c #x30 #x30 #x7e #x00 #x66 #x66 #x3c #x18 #x3c #x18 #x18 #x00
  #x1c #x36 #x66 #x7c #x66 #x66 #x7c #x60 #x1e #x30 #x7c #x30 #x30 #x30 #x60 #x00
  #x0c #x18 #x00 #x3c #x06 #x7e #x3e #x00 #x0c #x18 #x00 #x38 #x18 #x18 #x3c #x00
  #x0c #x18 #x00 #x3c #x66 #x66 #x3c #x00 #x0c #x18 #x00 #x66 #x66 #x66 #x3e #x00
  #x34 #x58 #x00 #x7c #x66 #x66 #x66 #x00 #x34 #x58 #x00 #x66 #x76 #x6e #x66 #x00
  #x00 #x3c #x06 #x3e #x66 #x3e #x00 #x3c #x00 #x3c #x66 #x66 #x66 #x3c #x00 #x3c
  #x00 #x18 #x00 #x18 #x30 #x60 #x66 #x3c #x00 #x00 #x00 #x3e #x30 #x30 #x30 #x00
  #x00 #x00 #x00 #x7c #x0c #x0c #x0c #x00 #xc6 #xcc #xd8 #x36 #x6b #xc3 #x86 #x0f
  #xc6 #xcc #xd8 #x36 #x6e #xd6 #x9f #x06 #x00 #x18 #x00 #x18 #x18 #x18 #x18 #x18
  #x1b #x36 #x6c #xd8 #x6c #x36 #x1b #x00 #xd8 #x6c #x36 #x1b #x36 #x6c #xd8 #x00
  #x34 #x58 #x00 #x3c #x06 #x7e #x3e #x00 #x34 #x58 #x00 #x3c #x66 #x66 #x3c #x00
  #x02 #x3c #x66 #x6e #x76 #x66 #x3c #x40 #x00 #x02 #x3c #x6e #x76 #x66 #x3c #x40
  #x00 #x00 #x7e #xdb #xdf #xd8 #x7e #x00 #x7f #xd8 #xd8 #xde #xd8 #xd8 #x7f #x00
  #x30 #x18 #x00 #x18 #x3c #x66 #x7e #x66 #x34 #x58 #x00 #x18 #x3c #x66 #x7e #x66
  #x34 #x58 #x3c #x66 #x66 #x66 #x66 #x3c #x66 #x00 #x00 #x00 #x00 #x00 #x00 #x00
  #x18 #x30 #x60 #x00 #x00 #x00 #x00 #x00 #x00 #x20 #x70 #x20 #x20 #x20 #x00 #x00
  #x7a #xca #xca #xca #x7a #x0a #x0a #x0a #x7e #xc3 #xbd #xb1 #xb1 #xbd #xc3 #x7e
  #x7e #xc3 #xbd #xa5 #xb9 #xad #xc3 #x7e #xf1 #x5b #x5f #x55 #x51 #x00 #x00 #x00
  #x66 #x00 #xe6 #x66 #x66 #xf6 #x06 #x1c #xf6 #x66 #x66 #x66 #x66 #xf6 #x06 #x1c
  #x00 #x66 #x76 #x3c #x6e #x66 #x00 #x00 #x00 #x7c #x0c #x0c #x0c #x7e #x00 #x00
  #x00 #x1e #x06 #x0e #x1e #x36 #x00 #x00 #x00 #x7e #x0c #x0c #x0c #x0c #x00 #x00
  #x00 #x7c #x06 #x66 #x66 #x66 #x00 #x00 #x00 #x70 #x30 #x30 #x30 #x30 #x00 #x00
  #x00 #x78 #x30 #x18 #x18 #x18 #x00 #x00 #x00 #x7e #x36 #x36 #x36 #x36 #x00 #x00
  #x60 #x6e #x66 #x66 #x66 #x7e #x00 #x00 #x00 #x78 #x18 #x18 #x00 #x00 #x00 #x00
  #x00 #x7c #x0c #x0c #x0c #x7c #x00 #x00 #x60 #x7e #x06 #x06 #x06 #x0e #x00 #x00
  #x00 #x6c #x3e #x66 #x66 #x6e #x00 #x00 #x00 #x38 #x18 #x18 #x18 #x78 #x00 #x00
  #x00 #x7c #x6c #x6c #x6c #x38 #x00 #x00 #x00 #x36 #x36 #x36 #x36 #x7e #x00 #x00
  #x00 #x7e #x66 #x76 #x06 #x7e #x00 #x00 #x00 #x66 #x66 #x3c #x0e #x7e #x00 #x00
  #x00 #x7c #x0c #x6c #x6c #x68 #x60 #x00 #x00 #x78 #x0c #x0c #x0c #x0c #x00 #x00
  #x00 #xd6 #xd6 #xd6 #xd6 #xfe #x00 #x00 #x00 #x7c #x6c #x6c #x6c #xec #x00 #x00
  #x00 #x70 #x30 #x30 #x30 #x30 #x30 #x00 #x00 #x7c #x0c #x0c #x0c #x0c #x0c #x00
  #x00 #xfe #x66 #x66 #x66 #x7e #x00 #x00 #x00 #x7e #x66 #x76 #x06 #x06 #x06 #x00
  #x00 #x6c #x6c #x38 #x18 #x18 #x18 #x00 #x0e #x1b #x3c #x66 #x66 #x3c #xd8 #x70
  #x00 #x10 #x38 #x6c #xc6 #x82 #x00 #x00 #x66 #xf7 #x99 #x99 #xef #x66 #x00 #x00
  #x00 #x00 #x76 #xdc #xc8 #xdc #x76 #x00 #x1c #x36 #x66 #x7c #x66 #x66 #x7c #x60
  #x00 #xfe #x66 #x62 #x60 #x60 #x60 #xf8 #x00 #x00 #xfe #x6c #x6c #x6c #x6c #x48
  #xfe #x66 #x30 #x18 #x30 #x66 #xfe #x00 #x00 #x1e #x38 #x6c #x6c #x6c #x38 #x00
  #x00 #x00 #x6c #x6c #x6c #x6c #x7f #xc0 #x00 #x00 #x7e #x18 #x18 #x18 #x18 #x10
  #x3c #x18 #x3c #x66 #x66 #x3c #x18 #x3c #x00 #x3c #x66 #x7e #x66 #x66 #x3c #x00
  #x00 #x3c #x66 #x66 #x66 #x24 #x66 #x00 #x1c #x36 #x78 #xdc #xcc #xec #x78 #x00
  #x0c #x18 #x38 #x54 #x54 #x38 #x30 #x60 #x00 #x10 #x7c #xd6 #xd6 #xd6 #x7c #x10
  #x3e #x70 #x60 #x7e #x60 #x70 #x3e #x00 #x3c #x66 #x66 #x66 #x66 #x66 #x66 #x00
  #x00 #x7e #x00 #x7e #x00 #x7e #x00 #x00 #x18 #x18 #x7e #x18 #x18 #x00 #x7e #x00
  #x30 #x18 #x0c #x18 #x30 #x00 #x7e #x00 #x0c #x18 #x30 #x18 #x0c #x00 #x7e #x00
  #x00 #x0e #x1b #x1b #x18 #x18 #x18 #x18 #x18 #x18 #x18 #x18 #xd8 #xd8 #x70 #x00
  #x18 #x18 #x00 #x7e #x00 #x18 #x18 #x00 #x00 #x32 #x4c #x00 #x32 #x4c #x00 #x00
  #x38 #x6c #x38 #x00 #x00 #x00 #x00 #x00 #x38 #x7c #x38 #x00 #x00 #x00 #x00 #x00
  #x00 #x00 #x00 #x00 #x60 #x60 #x00 #x00 #x00 #x00 #x0f #x18 #xd8 #x70 #x30 #x00
  #x38 #x6c #x6c #x6c #x6c #x00 #x00 #x00 #x38 #x6c #x18 #x30 #x7c #x00 #x00 #x00
  #x78 #x0c #x38 #x0c #x78 #x00 #x00 #x00 #x00 #xfe #x00 #x00 #x00 #x00 #x00 #x00)
