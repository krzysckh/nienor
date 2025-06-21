(define-simple-deo putchar #x18)
(define-simple-deo putchar-error #x19)

;; very very inefficient and local-stack exhausting way to print a number
(define (_print-number n depth)
  (if (equ? 1 (band (equ? n 0) (> depth 0)))
      (noop)
      (begin
        ;; (debug!)
        (_print-number (/ n 10) (+ depth 1)) ; ouch!
        (putchar (+ #\0 (modulo n 10))))))

(define-macro-rule ()
  (print-number n)
  (begin
    (_print-number n 0)
    (putchar #\newline)))

(define (puts-static ptr)
  (if (equ? (get8! ptr) 0)
      (noop)
      (begin
        (putchar (get8! ptr))
        (puts-static (+ ptr 1)))))

(define-macro-rule ()
  (puts s)
  (puts-static s))

(define-simple-deo2 set-draw-handler! #x20)
(define-simple-deo2 set-mouse-handler! #x90)
(define-simple-deo2 set-key-handler! #x80)

(define-simple-deo2 set-color-r! #x08)
(define-simple-deo2 set-color-g! #x0a)
(define-simple-deo2 set-color-b! #x0c)

(define-simple-deo2 set-screen-width! #x22)
(define-simple-deo2 set-screen-height! #x24)

(define (set-colors! r g b)
  (set-color-r! r)
  (set-color-g! g)
  (set-color-b! b))

(define-simple-deo2 pick-x! #x28)
(define-simple-deo2 pick-y! #x2a)

(define (pick-pixel! x y)
  (pick-x! x)
  (pick-y! y))

(define (pixel! x y color fill layer fx fy)
  (pick-pixel! x y)

  (bior* color fill layer fx fy)

  (with-locals! (mask)
    (short->byte mask)
    (pus! #x2e)
    (deo!)))

(define (fill! x y color layer)
  (pixel! x y color fill-mode layer 0 0))

(define (draw-pixel! x y color)
  (pixel! x y color 0 0 0 0))

(define-simple-deo2 pick-sprite! #x2c)

;; TODO: rewrite these in a modern way
;; i don't have to use the stack explicitly anymore
(define (sprite! x y sprite bpp2? layer fx fy color)
  (pick-pixel! x y)
  (pick-sprite! sprite)

  (bior* bpp2? layer fx fy color)

  (with-locals! (mask)
    (short->byte mask)
    (pus! #x2f)
    (deo!)))

(define (mouse-x)
  (pus! #x92)
  (dei2!))

(define (mouse-y)
  (pus! #x94)
  (dei2!))

(define (key-pressed)
  (pus! #x83)
  (dei2!))

(define (button-pressed)
  (pus! #x82)
  (dei2!)
  (with-locals! (b) ; Seriously? RET is a CR?
    (if (equ? b 13)
        #\newline
        b)))

(define (mouse-state)
  (pus! #x96)
  (dei!)
  (pus! 0)
  (uxn-call! () swp))

(define-simple-deo2 set-file/name! #xa8)
(define-simple-deo2 set-file/length! #xaa)
(define-simple-deo2 set-file/read! #xac)
(define-simple-deo2 set-file/write! #xae)

(define (file-success)
  (pus! #xa2)
  (dei2!))

(define *file-reader-chunk-size* 512)

(define (_do-read-file buf bufsize read)
  (let ((ptr (+ buf read)))
    (set-file/read! ptr)
    (let ((fs (file-success)))
      (set8! (+ ptr fs) 0)
      (if (equ? fs 0)
          buf
          (let* ((read*   (- (+ read *file-reader-chunk-size*) 1))
                 (newsize (+ *file-reader-chunk-size* bufsize)))
            (let ((buf2 (realloc buf newsize)))
              (_do-read-file buf2 newsize read*)))))))

;; filename-ptr → malloc'd ptr with full file. assumes file can fit in the ram
(define (read-file fname-ptr)
  (set-file/name! fname-ptr)
  (let ((ptr (malloc *file-reader-chunk-size*)))
    (set-file/length! (- *file-reader-chunk-size* 1))
    (set-file/name! fname-ptr)
    (_do-read-file ptr *file-reader-chunk-size* 0)))

(define (write-file fname-ptr data-ptr data-size)
  (set-file/length! data-size)
  (set-file/name! fname-ptr)
  (set-file/write! data-ptr))
