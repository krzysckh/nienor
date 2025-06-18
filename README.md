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
(defvar hello "Hello, World!\n" 0)

(define (main)
  (let ((puts puts-static))
    (puts (addrof hello))
    (exit 128)))
```

```sh
# compile hello.scm to hello.rom and run it
$ bin/nienor --optimize hello.scm -o hello.rom && uxnemu hello.rom
Assembled hello.rom in 919.00B (1.40% used), 51 labels
Hello, World!
Run: Ended.
# dump hello.scm as uxntal, compile it with uxnasm and run it
$ bin/nienor -o hello.tal -OD hello.scm && uxnasm hello.tal hello.rom && uxnemu hello.rom
Assembled hello.rom in 919 bytes(1.41% used), 0 labels, 0 macros.
Hello, World!
Run: Ended.
```
