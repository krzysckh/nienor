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
    (puts hello)
    (exit 128)))
```

```sh
# compile hello.scm to hello.rom and run it
$ bin/nienor --optimize hello.scm -o hello.rom && uxnemu hello.rom
Assembled hello.rom in 286.00B (0.44% used), 28 labels
Hello, World!
Run: Ended.
# dump hello.scm as uxntal, compile it with uxnasm and run it
$ bin/nienor -o hello.tal -OD hello.scm && uxnasm hello.tal hello.rom && uxnemu hello.rom
Assembled hello.rom in 286 bytes(0.44% used), 0 labels, 0 macros.
Hello, World!
Run: Ended.
```
