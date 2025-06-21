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
(define (main)
  (puts "Hello, World!\n")
  (exit!))
```

```sh
# compile hello.scm to hello.rom and run it
$ bin/nienor --optimize hello.scm -o hello.rom && uxnemu hello.rom
Assembled hello.rom in 966.00B (1.47% used), 59 labels
Hello, World!
Run: Ended.
# dump hello.scm as uxntal, compile it with uxnasm and run it
$ bin/nienor -o hello.tal -OD hello.scm && uxnasm hello.tal hello.rom && uxnemu hello.rom
Assembled hello.rom in 965 bytes(1.48% used), 0 labels, 0 macros.
Hello, World!
Run: Ended.
```
