(define-simple-deo _putchar #x18)
(define-simple-deo _putchar-error #x19)

(define (putchar c)
  (_putchar c))

(define (putchar-error c)
  (_putchar-error c))

(define-signature _print-number Number -> Number -> Void)
;; very very inefficient and local-stack exhausting way to print a number
(define (_print-number n depth)
  (when (not (and (eq? n 0) (> depth 0)))
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

(define-signature puts-static String -> Pointer -> Void)
(define (puts-static ptr putchar)
  (if (equ? (get8! ptr) 0)
      (noop)
      (begin
        (putchar (get8! ptr))
        (puts-static (+ ptr 1) putchar))))

(define-signature _putchar -> Number -> Void)
(define (_putchar c)
  (putchar c))

(define-signature _eputchar -> Number -> Void)
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
  (puts (typechecker-bogger-off! s)))

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

(define-signature set-colors! Number -> Number -> Number -> Void)
(define (set-colors! r g b)
  (set-color-r! r)
  (set-color-g! g)
  (set-color-b! b))

(define-simple-deo2 pick-x! #x28)
(define-simple-deo2 pick-y! #x2a)

(define-signature pick-pixel! Number -> Number -> Void)
(define (pick-pixel! x y)
  (pick-x! x)
  (pick-y! y))

(define-signature pixel! Number -> Number -> Number -> Number -> Number -> Number -> Number -> Void)
(define (pixel! x y color fill layer fx fy)
  (pick-pixel! x y)

  (bior* color fill layer fx fy)

  (with-locals! (mask)
    (short->byte mask)
    (pus! #x2e)
    (deo!)))

(define-signature fill! Number -> Number -> Number -> Number -> Void)
(define (fill! x y color layer)
  (pixel! x y color fill-mode layer 0 0))

(define-signature draw-pixel! Number -> Number -> Number -> Void)
(define (draw-pixel! x y color)
  (pixel! x y color 0 0 0 0))

(define-simple-deo2 pick-sprite! #x2c)

;; TODO: rewrite these in a modern way
;; i don't have to use the stack explicitly anymore
(define-signature sprite! Number -> Number -> Number -> Number -> Number -> Number -> Number -> Number -> Void)
(define (sprite! x y sprite bpp2? layer fx fy color)
  (pick-pixel! x y)
  (pick-sprite! sprite)
  (let ((mask (bior* (if bpp2? #b10000000 0) layer fx fy color)))
    (short->byte mask)
    (pus! #x2f)
    (deo!)))

(define-signature console-read Number)
(define (console-read)
  (pus! #x12)
  (dei!)
  (pus! 0)
  (uxn-call! () swp))

(define-signature console-type Number)
(define (console-type)
  (pus! #x17)
  (dei!)
  (pus! 0)
  (uxn-call! () swp))

(define-signature mouse-x Number)
(define (mouse-x)
  (pus! #x92)
  (dei2!))

(define-signature mouse-y Number)
(define (mouse-y)
  (pus! #x94)
  (dei2!))

(define-signature key-pressed Number)
(define (key-pressed)
  (pus! #x83)
  (dei2!))

(define-signature button-pressed Number)
(define (button-pressed)
  (pus! #x82)
  (dei2!)
  (with-locals! (b) ; Seriously? RET is a CR?
    (if (equ? b 13)
        #\newline
        b)))

(define-signature mouse-state Number)
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

(define-signature _do-read-file Pointer -> Number -> Number -> Pointer)
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
(define-signature read-file String -> Pointer)
(define (read-file fname-ptr)
  (set-file/name! fname-ptr)
  (let ((ptr (malloc *file-reader-chunk-size*)))
    (set-file/length! (- *file-reader-chunk-size* 1))
    (set-file/name! fname-ptr)
    (_do-read-file ptr *file-reader-chunk-size* 0)))

(define-signature write-file String -> Pointer -> Number -> Void)
(define (write-file fname-ptr data-ptr data-size)
  (set-file/length! data-size)
  (set-file/name! fname-ptr)
  (set-file/write! data-ptr))

(define-signature fill-rect! Number -> Number -> Number -> Number -> Number -> Number -> Number -> Void)
(define (fill-rect! sprite color x1 y1 x2 y2 layer)
  (loopn (i y1 y2 8)
    (loopn (j x1 x2 8)
      (sprite! j i sprite 0 layer 0 0 color))))

(define-signature error String -> Void)
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

;; TODO: define 2bpp sprite
(define-macro-rule (_)
  (_define-sprite acc _ . rest)
  (_define-sprite (<< acc 1) . rest))

(define-macro-rule (X)
  (_define-sprite acc X . rest)
  (_define-sprite (bior (<< acc 1) 1) . rest))

(define-macro-rule ()
  (_define-sprite acc)
  (__num->8-bytes acc))

(define-macro-rule ()
  (__num->8-bytes num)
  ((>> (band num #xff00000000000000) (- 64 8))
   (>> (band num #xff000000000000)   (- 64 (* 2 8)))
   (>> (band num #xff0000000000)     (- 64 (* 3 8)))
   (>> (band num #xff00000000)       (- 64 (* 4 8)))
   (>> (band num #xff000000)         (- 64 (* 5 8)))
   (>> (band num #xff0000)           (- 64 (* 6 8)))
   (>> (band num #xff00)             (- 64 (* 7 8)))
   (band num #xff)))

(define-macro-rule ()
  (define-sprite name . rest)
  (alloc! name . (_define-sprite 0 . rest)))

(define-macro-rule ()
  (define-2bpp-sprite name (a1 a2 a3 a4) . full)
  (with-gensyms (M)
    (flatten!
     (define-macro-rule ()
       (M (acc1 acc2))
       ((__num->8-bytes acc1) (__num->8-bytes acc2)))
     (define-macro-rule (a1)
       (M (acc1 acc2) a1 . rest)
       (M ((<< acc1 1) (<< acc2 1)) . rest))
     (define-macro-rule (a2)
       (M (acc1 acc2) a2 . rest)
       (M ((bior (<< acc1 1) 1) (<< acc2 1)) . rest))
     (define-macro-rule (a3)
       (M (acc1 acc2) a3 . rest)
       (M ((<< acc1 1) (bior (<< acc2 1) 1)) . rest))
     (define-macro-rule (a4)
       (M (acc1 acc2) a4 . rest)
       (M ((bior (<< acc1 1) 1) (bior (<< acc2 1) 1)) . rest))
     (_alloc! name (M (0 0) . full)))))
