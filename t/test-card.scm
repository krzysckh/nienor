(define *width*  #x170)
(define *height* #xe0)

(define *font-size* 12)

(defvar xor 98)
(defvar frame-ctr 0)

(defvar data-1)
(defvar data-2)
(defvar data-3)

(define (f x y)
  (bior
   (= x xor)
   (modulo (bxor x xor) y)))

(define (measure-full data)
  (if data
      (+ (uf2/measure-text font (cdr (car data))) (measure-full (cdr data)))
      0))

(define (print-data y data)
  (let ((loop (λ (loop data x) ; poor man's letrec w/ no tailcalls
                (when data
                  (let ((it (car data)))
                    (uf2/vputs font (+ x 1) (+ y 1) (cdr it) layer-0 color-4)
                    (uf2/vputs font x y (cdr it) layer-1 (car it))
                    (loop
                     loop
                     (cdr data)
                     (+ x (uf2/measure-text font (cdr it)))))))))
    (loop loop data (- *width* (measure-full data) 8))))

(alloc! surname "Micha" 134 "czyk" 0) ; <- hack, as owl really wants \x86; to be 2 bytes long

(define (prepare-card)
  (fill! 0 0 0 layer-0)
  (loopn (y 0 *height* 1)
    (loopn (x 0 *width* 1)
      (when (not (f x y))
        (draw-pixel! x y color-2))))
  (print-data (- *height* *font-size* 8) data-1)
  (print-data (- *height* *font-size* 8 *font-size* 8) data-2)
  (print-data (- *height* *font-size* 8 *font-size* 8 *font-size* 8) data-3)
  )

(define (main)
  (set-screen-size! *width* *height*)
  (set-colors! #x2de4 #x2d04 #x2d04)
  (print-number font)
  (set-draw-handler! draw)
  (set! data-1
        (list (cons color-2 "(")
              (cons color-3 "dot")
              (cons color-2 " 'krzysckh 'org)")))
  (set! data-2
        (list (cons color-2 "kpm")
              (cons color-3 "@")
              (cons color-2 "krzysckh.org")))
  (set! data-3
        (list (cons color-2 "Krzysztof ")
              (cons color-3 surname)))
  (prepare-card))

(define-vector (draw)
  (set! frame-ctr (+ frame-ctr 1))
  (when (>= frame-ctr 20)
    (set! xor (+ xor 1))
    (set! frame-ctr 0)
    (fill! 0 0 color-1 layer-0)
    (prepare-card)))

(_alloc! font (embed "res/courier12.uf2"))

(metadata!
 "Krzysztof Michalczyk" #\newline
 "kpm@krzysckh.org"     #\newline
 "(dot 'krzysckh 'org)" #\newline)
