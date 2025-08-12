;;;--- initialization

(codegen-at! #x100)

;; this gets called at #x100, this should contain only the most important stuff
;; to bootstrap the execution
;; that's why the compiler requires (main) to be defined, it's like _start in assemblers
;; ...or main in C
;; ...or WinMain in microsoft C

(set-metadata! *METADATA*)
(malloc/init)
(main)
(brk!)

;;--- end initialization

(define-label! ___alloc-arg)
  (uxn-call! (2) swp) ; we jumped here with JSR2k, so the address is kept, and we want to push the arg, not the address
  (pus! 0)
  (uxn-call! () ldz)  ; load ptr
  (uxn-call! () inc)  ; inc ptr
  (pus! 2)
  (uxn-call! () mul)  ; *2 because all pointers are shorts
  (uxn-call! (2) stz) ; store arg at ptr
  (pus! 0)
  (uxn-call! () ldz)
  (uxn-call! () inc) ; load & inc ptr again
  (pus! 0)
  (uxn-call! () stz) ; store new ptr at 0x0
  (uxn-call! (2 r) jmp)

;;- rest of prelude

(define-macro-rule ()
  (defun f (arg1 . args) . body)
  (_defun f (arg1 . args) body))

(define-macro-rule ()
  (defun f (arg1) . body)
  (_defun f (arg1) body))

(define-macro-rule ()
  (defun f () . body)
  (_defun f () body))

(define-macro-rule ()
  (define name value)
  (define-constant name value))

(define-macro-rule ()
  (define (f . arg) . body)
  (_defun f arg body))

(define-macro-rule ()
  (define-vector (f . args) . body)
  (_defun-vector f args body))

(define-macro-rule ()
  (push! it)
  (_push! _ it))

(define-macro-rule ()
  (pus! it)
  (_push! byte it))

(define-macro-rule ()
  (λ args . body)
  (_λ args body))

(define-macro-rule (())
  (λ () . body)
  (_λ () body))

(define-macro-rule ()
  (lambda . rest)
  (λ . rest))

(define-macro-rule ()
  (compiler-error . vs)
  (_compiler-error vs))

(define-macro-rule ()
  (deo!)
  (uxn-call! () deo))

(define-macro-rule ()
  (deo2!)
  (uxn-call! (2) deo))

(define-macro-rule ()
  (dei!)
  (uxn-call! () dei))

(define-macro-rule ()
  (dei2!)
  (uxn-call! (2) dei))

(define-macro-rule ()
  (brk!)
  (uxn-call! () brk))

(define-macro-rule ()
  (short->byte x)
  (_push! byte x))

(define-macro-rule ()
  (byte->short x)
  (begin
    x (pus! 0) (uxn-call! () swp)))

(define-macro-rule ()
  (alloc! name . vals)
  (_alloc! name vals))

(define-macro-rule ()
  (defvar name value)
  (flatten!
   (alloc! name (>> value 8) (band value #xff))
   (_declare-var! name)))

(define-macro-rule ()
  (defvar name)
  (defvar name 0))

(define-macro-rule ()
  (deo-with arg at)
  (begin
    arg (uxn-call! () nip)
    at  (uxn-call! () nip)
    (deo!)))

(define-macro-rule ()
  (deo2-with arg at)
  (begin
    arg (_push! byte at) (deo2!)))

(define-macro-rule ()
  (define-simple-deo name addr)
  (define-macro-rule ()
    (name _arg)
    (deo-with _arg addr)))

(define-macro-rule ()
  (define-simple-deo2 name addr)
  (define-macro-rule ()
    (name _arg)
    (deo2-with _arg addr)))

(define-simple-deo2 set-metadata! #x06)

(define-macro-rule ()
  (define-binop name uxn-op . modes)
  (flatten!
   (define-macro-rule ()
     (name arg1 arg2 . argn)
     (name (name arg1 arg2) . argn))

   (define-macro-rule (_)
     (name _arg1 _arg2)
     (begin
       _arg1 _arg2
       (uxn-call! modes uxn-op)))))

(define-macro-rule ()
  (with-label label . body)
  (_with-label label body))

(define-macro-rule ()
  (_declare-test . blah)
  (_flatten! ()))

(define-macro-rule ()
  (include! filename)
  (_include! filename))

(define color-1 0)
(define color-2 1)
(define color-3 2)
(define color-4 3)

(define fill-mode  #b10000000)
(define pixel-mode #b00000000)
(define layer-0    #b00000000)
(define layer-1    #b01000000)
(define flip-x     #b00100000)
(define flip-y     #b00010000)

(define bg-dot     #x00)
(define bg-fill-br #x80)
(define bg-fill-bl #x90)
(define bg-fill-tr #xa0)
(define bg-fill-tl #xb0)
(define fg-dot     #x40)
(define fg-fill-br #xc0)
(define fg-fill-bl #xd0)
(define fg-fill-tr #xe0)
(define fg-fill-tl #xf0)

(define-macro-rule ()
  (jmp2r!)
  (uxn-call! (2 r) jmp))

(define-macro-rule ()
  (__m_reverse)
  ())

(define-macro-rule (_)
  (__m_reverse a . b)
  (__m_reverse (_ . b) a))

(define-macro-rule (_)
  (__m_reverse (_) . a)
  a)

(define-macro-rule (_)
  (__m_reverse (_ b . c) . a)
  (__m_reverse (_ . c) b . a))

(define-macro-rule ()
  (debug!)
  (begin (pus! 1) (pus! #x0e) (deo!)))

(define-macro-rule ()
  (when exp . body)
  (if exp (begin . body) (begin)))

(define-binop + add 2)
(define-binop - sub 2)
(define-binop * mul 2)
(define-binop / div 2)
(define-binop band and 2)
(define-binop bior ora 2)
(define-binop bxor eor 2)

(define-macro-rule ()
  (and x . rest)
  (if x (and . rest) #f))

(define-macro-rule ()
  (and x)
  (if x x #f))

(define-macro-rule ()
  (or . rest)
  (bior . rest))

(define-macro-rule ()
  (bnot x)
  (bxor x #xffff))

(define-macro-rule ()
  (not x)
  (if x #f #t))

(define-signature > Any -> Any -> Bool)
(define (> a b)
  a b
  (uxn-call! (2) gth)
  (pus! 0)
  (uxn-call! () swp))

(define-signature < Any -> Any -> Bool)
(define (< a b)
  a b
  (uxn-call! (2) lth)
  (pus! 0)
  (uxn-call! () swp))

(define-macro-rule ()
  (>= a b)
  (bior (> a b) (equ? a b)))

(define-macro-rule ()
  (<= a b)
  (bior (< a b) (equ? a b)))

(define-macro-rule ()
  (bior* a b . c)
  (bior a (bior* b . c)))

(define-macro-rule ()
  (bior* a b)
  (bior a b))

(define-signature band8 Number -> Number -> Number)
(define (band8 a b)
  (pus! a)
  (pus! b)
  (uxn-call! () and))

(define-signature << Number -> Number -> Number)
(define (<< a b)
  (push! a)

  (pus! b)
  (pus! #x40)
  (uxn-call! () sft)

  (uxn-call! (2) sft))

(define-signature >> Number -> Number -> Number)
(define (>> a b)
  (push! a)
  (band8 b #b00001111)
  (uxn-call! (2) sft))

(define-simple-deo _exit #x0f)
(define (exit n)
  (_exit n))

(define-macro-rule ()
  (flatten! . body)
  (_flatten! body))

(define-macro-rule ()
  (exit!)
  (exit 128))

(define-signature equ? Any -> Any -> Bool)
;; short equal
(define (equ? a b)
  (push! a)
  (push! b)
  (uxn-call! (2) equ)
  (pus! 0)
  (uxn-call! () swp))

(define-macro-rule ()
  (= a b)
  (equ? a b))

(define-macro-rule ()
  (eq? a b)
  (equ? a b))

(define-signature _get! Pointer -> Any)
(define-macro-rule ()
  (get! addr)
  (_get! addr))

(define-macro-rule ()
  (set! place value)
  (begin
    (push! value)
    (push! (addrof place))
    (uxn-call! (2) sta)))

(define-macro-rule ()
  (addrof var)
  (_addrof var))

(define-signature get8! Pointer -> Any)
(define (get8! place)
  (typechecker-bogger-off!
   (get! place)
   (uxn-call! () pop)
   (pus! 0)
   (uxn-call! () swp)))

(define-macro-rule ()
  (set8! place value)
  (begin
    value (uxn-call! () nip)
    (push! (addrof place))
    (uxn-call! () sta)))

(define-macro-rule ()
  (jmp! to)
  (begin
    to (uxn-call! (2) jmp)))

(define-macro-rule ()
  (with-locals! names . body)
  (_with-locals! names body))

(define-macro-rule ()
  (with-gensyms (exp . rest) . body)
  (with-gensym exp (with-gensyms rest . body)))

(define-macro-rule ()
  (with-gensyms (exp) . body)
  (with-gensym exp . body))

(define-macro-rule ()
  (loopn (it _from _to _by) . body)
  (with-gensyms (to by)
    (let ((it _from)
          (to _to)
          (by _by))
      (with-label _loop
        (when (> to it)
          (begin . body)
          (set! it (+ it by))
          (jmp! _loop))))))

(define-macro-rule ()
  (in-epilogue! . body)
  (_in-epilogue! body))

;; (define-macro-rule ()
;;   (while exp . body)
;;   (with-label _while
;;     (when exp
;;       (begin . body)
;;       (jmp! _while))))

(define-macro-rule ()
  (with-local k v . body)
  (begin
    v (with-locals! (k) . body)))

(define-macro-rule ()
  (let* ((key val) . rest) . body)
  (with-local key val (let* rest . body)))

(define-macro-rule ()
  (let* ((key val)) . body)
  (with-local key val . body))

(define-macro-rule (_)
  (let ((key val) . rest) . body)
  (let (_ key) (_ val) rest . body))

(define-macro-rule (_)
  (let (_ . keys) (_ . values) ((key val) . rest) . body)
  (let (_ key . keys) (_ val . values) rest . body))

(define-macro-rule ()
  (begin . code)
  (_begin code))

(define-macro-rule ()
  (nigeb . vs)
  (begin . (__m_reverse . vs)))

(define-macro-rule (_ ())
  (let (_ . keys) (_ . values) () . body)
  (begin
    (nigeb . values)
    (with-locals! (__m_reverse . keys) . body)))

(define (modulo a mod)
  (- a (* (/ a mod) mod)))

(define-macro-rule ()
  (noop)
  (begin))

;; https://github.com/krzysckh/goofing/blob/master/uxn-helpers.c
;; https://en.wikipedia.org/wiki/Xorshift
(alloc! *rand-seed-state* 8 91)

(define-signature rand Number)
(define (rand)
  (let* ((x (get! *rand-seed-state*))
         (x (bxor x (<< x 3)))
         (x (bxor x (>> x 7)))
         (x (bxor x (<< x 1))))
    (set! *rand-seed-state* x)
    x))

(define-signature srand Number -> Void)
(define (srand x)
  (set! *rand-seed-state* x))

(define-macro-rule ()
  (min2 a b)
  (if (< a b) a b))

(define-macro-rule ()
  (max2 a b)
  (if (> a b) a b))

(define-macro-rule (as)
  (with exp as name . body)
  (let ((name exp))
    (begin . body)
    (free name)))

(define-signature memcpy Pointer -> Pointer -> Number -> Void)
(define (memcpy dst src len)
  (loopn (i 0 len 1)
    (set8! (+ dst i) (get8! (+ src i)))))

(define-signature _find-chr Pointer -> Number -> Any -> Number)
(define (_find-chr p n chr)
  (if (bior (equ? (get8! p) 0) (equ? (get8! p) chr))
      n
      (_find-chr (+ p 1) (+ n 1) chr)))

(define (_strlen p n)
  (_find-chr p n 0))

(define-macro-rule ()
  (strlen p)
  (_strlen p 0))

(define-macro-rule ()
  (find-char p chr)
  (_find-chr p 0 chr))

(define-macro-rule ()
  (strchr p chr)
  (find-char p chr))

;; useful for displaying multiline text
(define-macro-rule ()
  (call-with-lines f str)
  (_call-with-lines f str 0))

(define-signature _call-with-lines Pointer -> Pointer -> Number -> Void)
;; f = (λ (s counter) ...)
(define (_call-with-lines f str ctr)
  (when (not (equ? (strlen str) 0))
    (let* ((len (find-char str #\newline))
           (p (malloc (+ len 1))))
      (memcpy p str len)
      (set8! (+ p len) 0)
      (f p ctr)
      (free p)
      (_call-with-lines f (+ str len 1) (+ ctr 1)))))

(define-signature string-append String -> String -> String)
(define (string-append a b)
  (let* ((la (strlen a))
         (lb (strlen b))
         (p (malloc (+ la lb 1))))
    (memcpy p a la)
    (memcpy (+ p la) b (+ lb 1))
    (typechecker-bogger-off! p))) ; <- TODO: should String == Pointer?

(define-signature string=? String -> String -> Bool)
(define (string=? a b)
  (let ((ca (get8! a))
        (cb (get8! b)))
    (if (= ca cb)
        (if (= ca 0)
            #t
            (string=? (+ a 1) (+ b 1)))
        #f)))

(define-signature _expt Number -> Number -> Number -> Number)
(define (_expt _a b a)
  (cond
   ((= b 0) 1)
   ((= b 1) _a)
   (else
    (_expt (* _a a) (- b 1) a))))

(define-macro-rule ()
  (expt a b)
  (_expt a b a))

(define-macro-rule ()
  (_make-1s n)
  (- (expt 2 n) 1))

(define-macro-rule ()
  (cond (a . b) . rest)
  (if a (begin . b) (cond . rest)))

(define-macro-rule ()
  (cond (a . b))
  (if a (begin . b) #f))

(define-macro-rule (else)
  (cond (else . code))
  (cond (#t . code)))

;; this is fucking hilarious
;; _symbol is not a funcion, but the typechecker will treat it as such
(define-signature _symbol Any -> Symbol)
(define-macro-rule ()
  (symbol x)
  (_symbol x))

(define-macro-rule ()
  (quote x)
  (symbol x))

(define-macro-rule (42)
  (define-struct name . rest)
  (flatten!
   (define-type name)
   (define-typing-rules
     (name is Pointer)
     (Pointer is name))
   (define-struct (42 0) name . rest)))

(define-macro-rule (42)
  (define-struct (42 size) name)
  (flatten!
   (define-macro-rule (name quote)
     (make-instance (quote name))
     ((__append-symbols make- name)))

   (define-signature (__append-symbols make- name) name)
   (define ((__append-symbols make- name))
     (malloc size))))

(define-macro-rule (42)
  (define-struct (42 size) name thing . rest)
  (flatten!
   (define-macro-rule ()
     ((__append-symbols name - thing) struct)
     (get! (+ struct size)))

   (define-macro-rule ()
     ((__append-symbols set- name - thing !) struct value)
     (set! (+ struct size) value))
   (define-struct (42 (+ size 2)) name . rest)))

(define-macro-rule (42)
  (define-struct (42 size) name (thing pack-size) . rest)
  (_define-packed-struct (42 size 0) name (thing pack-size) . rest))

(define-macro-rule (42)
  (_define-packed-struct (42 a b) name thing . rest)
  (_define-packed-struct (42 a b) name (thing 16) . rest))

(define-macro-rule ()
  (__pad-of size)
  (- size (* 16 (/ size 16))))

(define-macro-rule ()
  (__point-of struct base size)
  (+ struct base (* 2 (/ size 16))))

(define-macro-rule (42)
  (_define-packed-struct (42 base size) name (thing pack-size) . rest)
  ;;                              ^^^^- additional size in bits after base
  (flatten! ; TODO: i wonder if it's better to have it as macros or functions, they're quite big
   (define-signature (__append-symbols name - thing) name -> Any)
   (define ((__append-symbols name - thing) struct)
     (if (> pack-size (- 16 (__pad-of size)))
         (compiler-error "Unaligned struct" name "at item" thing "with size" pack-size)
         (band (>> (get! (+ struct base (* 2 (/ size 16))))
                   (- (- 16 (__pad-of size)) pack-size))
               (_make-1s pack-size))))

   (define-signature (__append-symbols set- name - thing !) name -> Any -> Void)
   (define ((__append-symbols set- name - thing !) struct value)
     (if (> pack-size (- 16 (__pad-of size)))
         (compiler-error "Unaligned struct" name "at item" thing "with size" pack-size)
         (set!
          (__point-of struct base size)
          (bior
           (band (get! (__point-of struct base size)) (bnot (<< (_make-1s pack-size)
                                                                (- (- 16 pack-size) (__pad-of size))))) ; clear value
           (<< (band value (_make-1s pack-size)) (- (- 16 pack-size) (__pad-of size)))))))

   (_define-packed-struct (42 base (+ size pack-size)) name . rest)))

(define-macro-rule (42)
  (_define-packed-struct (42 base size) name)
  (flatten!
   (define-macro-rule (name quote)
     (make-instance (quote name))
     ((__append-symbols make- name)))

   (define-signature (__append-symbols make- name) name)
   (define ((__append-symbols make- name))
     (malloc (+ base (/ size 8) 1)))))

(define-struct Cell car cdr)

(define nil 0)

(define (cons a b)
  (let ((pt (make-Cell)))
    (set-Cell-car! pt a)
    (set-Cell-cdr! pt b)
    pt))

(define-macro-rule ()
  (set-car! . args)
  (set-Cell-car! . args))

(define-macro-rule ()
  (set-cdr! . args)
  (set-Cell-cdr! . args))

(define-macro-rule ()
  (list a . rest)
  (cons a (list . rest)))

(define-macro-rule ()
  (list a)
  (cons a nil))

(define-macro-rule ()
  (car v)
  (Cell-car v))

(define-macro-rule ()
  (cdr v)
  (Cell-cdr v))

(define-signature _print-list Pointer -> Void)
(define (_print-list l)
  (when l
    (print-number* (car l))
    (when (cdr l) (putchar #\space))
    (_print-list (cdr l))))

(define-macro-rule ()
  (print-list l)
  (begin
    (putchar #\()
    (_print-list l)
    (putchar #\))
    (putchar #\newline)))

(define-macro-rule (_)
  (_let-loop name (_ . keys) (_ . vals) (_ (key value) . rest) . body)
  (_let-loop name (_ key . keys) (_ value . vals) (_ . rest) . body))

;; TODO: this grows the return stack — it doesn't tailcall D:
;; TODO: this doesn't return the right value
(define-macro-rule (_)
  (_let-loop name (_ . keys) (_ . vals) (_) . body)
  (begin
    (begin . vals)
    (with-label name
      (with-locals! keys
        . body))))

(define-macro-rule (_)
  (let loop ((key val) . rest) . body)
  (_let-loop loop (_ key) (_ val) (_ . rest) . body))

(define-signature free-list Pointer -> Void)
(define (free-list l)
  (when l
    (if (cdr l)
        (free-list (cdr l))
        (free l))))

(define-signature _length Pointer -> Number -> Number)
(define (_length l n)
  (if l
      (_length (cdr l) (+ n 1))
      n))

(define-macro-rule ()
  (length l)
  (_length l 0))

(define-macro-rule ()
  (pointer->string x)
  (typechecker-bogger-off! x))

(define-signature _list->string Pointer -> Pointer -> Number -> Void)
(define (_list->string l p n)
  (when l
    (set8! (+ p n) (car l))
    (_list->string (cdr l) p (+ n 1))))

(define-signature list->string Pointer -> String)
(define (list->string l)
  (let ((p (malloc (+ (length l) 1))))
    (_list->string l p 0)
    (pointer->string p)))

;; (define (set-car! cell what)
;;   (set! cell what))

;; (define (set-cdr! cell what)
;;   (set! (+ cell 2) what))

(define-signature _reverse Pointer -> Pointer -> Pointer)
(define (_reverse l acc)
  (if l
      (_reverse (cdr l) (cons (car l) acc))
      acc))

(define-macro-rule ()
  (reverse l)
  (_reverse l nil))

(define-macro-rule ()
  (reverse/ l_)
  (let* ((l l_)
         (v (reverse l)))
    (free-list l)
    v))

(define-signature for-each Pointer -> Pointer -> Void)
(define (for-each f lst)
  (when lst
    (f (car lst))
    (for-each f (cdr lst))))

(define-signature map Pointer -> Pointer -> Pointer)
(define (map f lst)
  (if lst
      (cons (f (car lst)) (map f (cdr lst)))
      nil))

(define-macro-rule ()
  (l/ f lst)
  (let* ((l lst) (v (f l)))
    (free-list l)
    v))

(define-macro-rule ()
  (f/ a b lst_)
  (let* ((l lst) (v (a b l)))
    (free-list l)
    v))

(define-macro-rule ()
  (map/ f l)
  (f/ map f l))

(define-signature append Pointer -> Pointer -> Pointer)
(define (append a b)
  (if a
      (cons (car a) (append (cdr a) b))
      b))

(define-macro-rule ()
  (append/ a_ b_)
  (let ((a a_) (b b_))
    (let ((l (append a b)))
      (free-list a)
      (free-list b)
      l)))

(define-macro-rule (_)
  (define-signature func . begin)
  (define-signature func (_) . begin))

(define-macro-rule (_)
  (define-signature func (_ . types) ret)
  (_define-signature func (__m_reverse . types) ret))

(define-macro-rule (-> _)
  (define-signature func (_ . types) t1 -> . rest)
  (define-signature func (_ t1 . types) . rest))

(define-macro-rule ()
  (metadata! . strings)
  (in-epilogue!
   (flatten!
    (alloc! *METADATA* 0 . strings)
    (nalloc! *METADATA-FINISH* 1))))

(define-macro-rule ()
  (typechecker-bogger-off! . rest)
  (_typechecker-bogger-off! rest))

;; TODO: actually define types, and not just let any symbol pass
(define-macro-rule ()
  (define-type T)
  (flatten!))

(define-macro-rule ()
  (define-typing-rules rule . rules)
  (flatten!
   (define-typing-rule . rule)
   (define-typing-rules . rules)))

(define-macro-rule ()
  (define-typing-rules)
  (flatten!))

(define-macro-rule (is <=>)
  (define-typing-rule T1 binding is T2 <=> exp)
  (_define-typing-rule T1 T2 binding exp))

(define-macro-rule (is <=>)
  (define-typing-rule T1 is T2)
  (_define-typing-rule T1 T2 _ #t))
