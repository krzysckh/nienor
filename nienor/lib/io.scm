(define-simple-deo putchar #x18)
(define-simple-deo putchar-error #x19)

(define-signature _print-number Number -> Number -> Void)
;; very very inefficient and local-stack exhausting way to print a number
(define (_print-number n depth)
  (if (equ? 1 (band (equ? n 0) (> depth 0)))
      (noop)
      (begin
        (_print-number (/ n 10) (+ depth 1)) ; ouch!
        (putchar (+ #\0 (modulo n 10))))))

(define-macro-rule ()
  (print-number n)
  (begin
    (_print-number n 0)
    (putchar #\newline)))

(define-macro-rule ()
  (print-number* n)
  (begin
    (_print-number n 0)))

(define (puts-static ptr putchar)
  (if (equ? (get8! ptr) 0)
      (noop)
      (begin
        (putchar (get8! ptr))
        (puts-static (+ ptr 1) putchar))))

(define (_putchar c)
  (putchar c))

(define (_eputchar c)
  (putchar-error c))

(define-macro-rule ()
  (puts s)
  (puts-static s _putchar))

(define-macro-rule ()
  (eputs s)
  (puts-static s _eputchar))

(define-macro-rule ()
  (print s)
  (puts s))

(define-simple-deo2 set-draw-handler! #x20)
(define-simple-deo2 set-mouse-handler! #x90)
(define-simple-deo2 set-key-handler! #x80)
(define-simple-deo2 set-console-handler! #x10)

(define-simple-deo2 set-color-r! #x08)
(define-simple-deo2 set-color-g! #x0a)
(define-simple-deo2 set-color-b! #x0c)

(define-simple-deo2 set-screen-width! #x22)
(define-simple-deo2 set-screen-height! #x24)

(define-macro-rule ()
  (set-screen-size! w h)
  (begin
    (set-screen-width! w)
    (set-screen-height! h)))

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
  (let ((mask (bior* bpp2? layer fx fy color)))
    (short->byte mask)
    (pus! #x2f)
    (deo!)))

(define (console-read)
  (pus! #x12)
  (dei!)
  (pus! 0)
  (uxn-call! () swp))

(define (console-type)
  (pus! #x17)
  (dei!)
  (pus! 0)
  (uxn-call! () swp))

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

(define (fill-rect! sprite color x1 y1 x2 y2 layer)
  (loopn (i y1 y2 8)
    (loopn (j x1 x2 8)
      (sprite! j i sprite 0 layer 0 0 color))))

(define (error s)
  (eputs s)
  (eputs "\n")
  (exit 129)
  (brk!))


(defvar *current-command-line-arg*)
(defvar *command-line*)
(defvar *command-line-cont*)

(define-vector (command-line-args-vec)
  (let ((type (console-type)))
    (cond
     ((= type 2)
      (set! *current-command-line-arg* (cons (console-read) *current-command-line-arg*)))
     ((bior (= type 3) (= type 4))
      (set! *command-line* (cons (reverse *current-command-line-arg*) *command-line*))
      (free-list *current-command-line-arg*)
      (set! *current-command-line-arg* nil))
     (else
      (print "type:")
      (print-number type)
      (error "WTF (whats that functionality)")))
    (when (= type 4)
      (let ((cl (l/ reverse (map list->string *command-line*))))
        (for-each free-list *command-line*)
        (free-list *command-line*)
        (set! *command-line* cl)
        (set-console-handler! nil)
        (when *command-line-cont*
          (*command-line-cont* *command-line*))))))

(define-macro-rule ()
  (has-command-line-arguments?)
  (= (console-type) 1))

(define-macro-rule ()
  (call-with-command-line f)
  (begin
    (set! *command-line-cont* f)
    (set-console-handler! command-line-args-vec)))
