;; inefficient memorywise as hell
(define-struct bunny color x y vx vy next)

;; TODO: snprintf (or some format function targetting a string)

(define w 400)
(define h 300)

(alloc! *bun*
  #b01000010
  #b01000010
  #b01111110
  #b01011010
  #b01111110
  #b01111111
  #b01111111
  #b00100100)

(alloc! *mouse*
  #b10000000
  #b11000000
  #b11100000
  #b11110000
  #b01100000
  #b00000000
  #b00000000
  #b00000000)

(defvar first-bun)
(defvar n-bnus)

(define (make-bunny*)
  (let ((b (make-bunny)))
    (when (= b 0)
      (error "Out of memory"))
    (puts "bun@") (print-number b)
    (set-bunny-x!     b (+ 1 (modulo (rand) (- w 10))))
    (set-bunny-y!     b (+ 1 (modulo (rand) (- h 10))))
    (set-bunny-vx!    b (+ 1 (modulo (rand) 6)))
    (set-bunny-vy!    b (+ 1 (modulo (rand) 6)))
    (set-bunny-color! b (+ 1 (modulo (rand) 3)))
    (set-bunny-next! b #x0000)
    b))

(define (main)
  (set-screen-size! w h)
  (set-draw-handler! draw)
  (set-key-handler! key)
  (set-colors! #x0f00 #x00f0 #x000f)
  (set! first-bun (make-bunny*)))

(define (add-bunny begin)
  (if (bunny-next begin)
      (add-bunny (bunny-next begin))
      (begin
        (set! n-bnus (+ n-bnus 1))
        (set-bunny-next! begin (make-bunny*)))))

(define (draw-bunnies bun)
  (when bun
    (let ((x (bunny-x bun))
          (y (bunny-y bun)))
      (let ((nx (signed+ x (bunny-vx bun)))
            (ny (signed+ y (bunny-vy bun))))
        (cond
         ((bior (signed>= (signed+ nx 8) w)
                (signed<= nx 0))
          (set-bunny-vx! bun (signed* (negative! 1) (bunny-vx bun)))
          (draw-bunnies bun))
         ((bior (signed>= (signed+ ny 8) h)
                (signed<= ny 0))
          (set-bunny-vy! bun (signed* (negative! 1) (bunny-vy bun)))
          (draw-bunnies bun))
         (else
          (sprite! x y *bun* 0 layer-0 0 0 (bunny-color bun))
          (set-bunny-x! bun nx)
          (set-bunny-y! bun ny)
          (draw-bunnies (bunny-next bun))))))))

(define-vector (draw)
  (when (mouse-state)
    (add-bunny first-bun))
  (fill! 0 0 color-1 layer-0)
  (draw-bunnies first-bun)
  (sprite! (mouse-x) (mouse-y) *mouse* 0 layer-0 0 0 color-2))

(define-vector (key)
  (loopn (i 0 100 1)
    (add-bunny first-bun)))
