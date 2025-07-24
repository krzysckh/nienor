(import
 (owl toplevel)
 (owl format)
 (prefix (nienor common) N/)
 (prefix (owl sys) sys/))

(define *tmp-rom-location* "/tmp/test.rom")
(define *uxnemu-implementation* (or (sys/getenv "UXNEMU") "uxn11"))

(define files
  (sort
   string-ci<?
   (map
    (H str "t/")
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

;; filename → (values filename possible-tests passed-tests)
(define (run-test filename)
  (if-lets ((data (assoc '_declare-test (N/file->sexps filename)))
            (ff (output-exp->ff data)))
    (values 2 (compile-and-expect filename (get ff 'output "")))
    (values 1 (compile filename))))

(define (print-outcome failed possible got)
  (format stdout "~%~a tests out of ~a ran successfully~%" got possible)
  (when (not (null? failed))
    (format stdout "failed: ~a~%" failed))
  (halt (if (= possible got) 0 42)))

(system '("make" "bin/nienor"))

(let loop ((files files) (failed #n) (max 0) (cur 0))
  (if (null? files)
      (print-outcome failed max cur)
      (lets ((possible got (run-test (car files))))
        (format stdout "~a: ~a/~a~%" (car files) got possible)
        (loop (cdr files)
              (if (= possible got) failed (cons (car files) failed))
              (+ max possible) (+ cur got)))))
