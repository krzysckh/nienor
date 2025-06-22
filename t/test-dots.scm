(define w 200)
(define h 200)

(defvar blayer)
(defvar layer)

;; (x^y)%5
(define (f1 x y)
  (modulo (bxor x y) 5))

;; (x^y)%7
(define (f2 x y)
  (modulo (bxor x y) 7))

;; (x^y)%9
(define (f3 x y)
  (modulo (bxor x y) 9))

;; x&y
(define (f4 x y)
  (band x y))

;; (x|y)%5
(define (f5 x y)
  (modulo (bior x y) 5))

;; (x^100)%y
(define (f6 x y)
  (modulo (bxor x 100) y))

(alloc! funcs f1 f2 f3 f4 f5 f6)
(define-label! end-funcs)

(defvar current-func)

(define (dp! x y)
  (pixel! x y color-2 0 layer-0 0 0))

(define (main)
  (puts "Press space to switch functions.\n")
  (set-screen-size! w h)
  (set-colors! #x0fff #x0fff #x0fff)
  (set-draw-handler! draw)
  (set-key-handler! key)
  (set! blayer (/ w 2))
  (set! layer blayer)
  (set! current-func funcs))

(define-vector (draw)
  (if (< layer #xffff)
      (begin ; before underflow
        (loopn (i layer (+ blayer (- blayer layer)) 1)
          (loopn (j layer (+ blayer (- blayer layer)) 1)
            (when (not ((get! current-func) i j))
              (dp! i j))))
        (set! layer (- layer 1)))
      (set-draw-handler! 0)))

(define-vector (key)
  (when (= (button-pressed) #\space)
    (set! layer blayer)
    (fill! 0 0 color-1 layer-0)
    (set-draw-handler! draw)
    (set! current-func (+ current-func 2))
    (when (>= current-func end-funcs)
      (set! current-func funcs))))
