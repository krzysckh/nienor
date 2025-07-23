(import
 (owl toplevel)
 (owl format)
 (prefix (nienor common) N/)
 (prefix (owl sys) sys/))

(define *self* "run-tests.scm")
(define *tmp-rom-location* "/tmp/test.rom")
(define *uxnemu-implementation* (or (sys/getenv "UXNEMU") "uxn11"))

(define files
  (map
   (H str "t/")
   (filter
    (B not (C equal? *self*))
    (filter
     (string->regex "m/\\.scm$/")
     (sys/dir->list "t")))))

(define (output-exp->ff exp)
  (let loop ((ff empty) (exp (cdr exp)))
    (if (null? exp)
        ff
        (if (eq? (cadr exp) '=>)
            (loop (put ff (car exp) (caddr exp)) (cdddr exp))
            (N/error "invalid test expression " exp)))))

(define (compile filename)
  (if (system `("./bin/nienor" "-qo" ,*tmp-rom-location* ,filename))
      1
      0))

(define (expect what)
  (lets ((r w (popen (format #f "~a ~a 2>/dev/null" *uxnemu-implementation* *tmp-rom-location*))))
    (if (equal? what (bytes->string (force-ll (port->byte-stream r)))) 1 0)))

(define (compile-and-expect filename what)
  (if (= 1 (compile filename))
      (+ 1 (expect what))
      1))

;; filename → (values possible-tests passed-tests)
(define (run-test filename)
  (if-lets ((data (assoc '_declare-test (N/file->sexps filename)))
            (ff (output-exp->ff data)))
    (values 2 (compile-and-expect filename (get ff 'output "")))
    (values 1 (compile filename))))

(define (print-outcome v)
  (lets ((possible got v))
    (format stdout "~%~a tests out of ~a ran successfully~%" got possible)
    (halt (if (= possible got) 0 42))))

(system '("make" "bin/nienor"))

(print-outcome
 (fold
  (λ (a b)
    (lets ((max cur a)
           (possible got (run-test b)))
      (format stdout "~a: ~a/~a~%" b got possible)
      (cons (+ max possible) (+ cur got))))
  (cons 0 0)
  files))
