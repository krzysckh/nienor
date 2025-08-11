# niënor

a lispy environment for uxn

## building

install [owl lisp](https://gitlab.com/owl-lisp/owl.git)

```sh
$ make all
$ ./bin/nienor -h
```

If you don't wish to build it from source, you could try the [web demo](https://pub.krzysckh.org/nienor-web-demo).
It's the same thing, just running in wasm. I don't promise to keep it up to date with @master though.

## examples

### Hello, World!

```scheme
(define (main)
  (puts "Hello, World!\n")
  (exit!))
```

```sh
# compile hello.scm to hello.rom and run it
$ bin/nienor hello.scm -o hello.rom && uxnemu hello.rom
Assembled hello.rom in 966.00B (1.47% used), 59 labels
Hello, World!
Run: Ended.
# dump hello.scm as uxntal, compile it with uxnasm and run it
$ bin/nienor -o hello.tal -D hello.scm && uxnasm hello.tal hello.rom && uxnemu hello.rom
Assembled hello.rom in 965 bytes(1.48% used), 0 labels, 0 macros.
Hello, World!
Run: Ended.
```

### Varvara wrappers

```scheme
(define (main)
  (set-colors! #xffff #x0000 #x000)
  (set-draw-handler! on-draw))

(define-vector (on-draw)
  (fill! 0 0 color-1 layer-0))
```

### Macros

```scheme
(alloc! spr #xf0 #xf0 #xf0 #xf0 #x0f #x0f #x0f #x0f)

(define-macro-rule (being execute) ; <- being and execute are literals
  (with-draw-handler being f execute . body)
  (begin
    (set-draw-handler! f) . body))

(define (main)
  (with-draw-handler being draw execute
    (set-screen-width! 400)
    (set-screen-height! 300)
    (set-colors! #x0f00 #x00f0 #x000f)
    (print-number 42)))

(define-vector (draw)
  (loopn (i 0 4 1)
    (fill-rect! spr i (* i 100) 0 (+ 100 (* i 200)) 300 0)))
```

### Anonymous functions

```scheme
(alloc! *str* "test\n")

(define (apply1 f arg)
  (f arg))

(define (main)
  (let ((printc (λ (c) (putchar c))))
    (loopn (i 0 5 1)
      (apply1 printc (get8! (+ *str* i)))))) ; <- get8! is a byte dereference.
                                             ; get! would dereference a short
```

### Closures

```scheme
(define (make-adder a)
  (λ (b)
    (λ (c)
      (+ a b c)))) ; <- vararg + (it's a macro)

(define (main)
  (let ((f ((make-adder 21) 10)))
    (print-number (f 11))
    (exit 128)))
```

### TCO

```scheme
(define-signature test-call-stack Number -> Number)
(define (test-call-stack n)
  ;; (print-number n) ; <- uncomment this to get a countdown from #xffff to 0
  (if (equ? n 0)
      0
      (test-call-stack (- n 1))))

(define (main)
  (print-number (test-call-stack #xffff))
  (exit 128))
```

### Lists

Lists are implemented but require manual memory management. Functions that operate on lists
might have counterparts that free the arguments after usage.

```scheme
(let ((l (append (list 1 2 3) (list 4))))
  (print-list l)) ; => (1 2 3 4)
;; This would leak memory allocated for (list 1 2 3) and (list 4),
;; as `append' creates a new list.

(let ((l (append/ (list 1 2 3) (list 4))))
  (print-list l)) ; => (1 2 3 4)
;; The append/ function runs append, and frees the args passed to it. If you only wish to free
;; one of the arguments, you have to manage the memory yourself. To free a list use (free-list ...)
```

### Type checking

There's a simple typechecker implemented that can catch some errors at compile-time.
If it's misbehaving, it may be fully disabled with `--disable-typechecker`.

```scheme
(define-signature f Number -> String)
(define (f x)
  x)

(define (main)
  (f 0)
  (exit!))
```
