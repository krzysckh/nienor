# niënor

a lispy environment for uxn

## building

install [owl lisp](https://gitlab.com/owl-lisp/owl.git)

```sh
$ make all
$ ./bin/nienor -h
```

## examples

```scheme
(alloc! hello "Hello, World!\n" 0)

(define (puts ptr)
  (if (equ? (get8! ptr) 0)
      #t
      (begin
        (putchar (get8! ptr))
        (puts (+ ptr 1)))))

(define (main)
  (puts hello)
  (exit 128))
```

```sh
# compile hello.scm to hello.rom and run it
$ bin/nienor -O hello.scm -o hello.rom && uxnemu hello.rom
Assembled to 287.00B
Hello, World!
Run: Ended.
# dump hello.scm as uxntal, compile it with uxnasm and run it
$ bin/nienor -o hello.tal -D hello.scm && uxnasm hello.tal hello.rom && uxnemu hello.rom
Assembled hello.rom in 287 bytes(0.44% used), 0 labels, 0 macros.
Hello, World!
Run: Ended.
```
