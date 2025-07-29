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
  (let loop ((vs empty) (exp (cdr exp)))
    (if (null? exp)
        vs
        (if (eq? (cadr exp) '=>)
            (loop (put vs (car exp) (caddr exp)) (cdddr exp))
            (N/error "invalid test expression " exp)))))

(define (compile filename vs)
  (if (system `("./bin/nienor" "-qo" ,*tmp-rom-location* ,filename))
      1
      (if (eq? 'failure (get vs 'expected-compilation-result #f))
          (begin
            (format stdout " ^- this is fine, as the 'expected-compilation result of this test was set to ~a~%"
                    (str* (get vs 'expected-compilation-result #f)))
            1)
          0)))

(define (expect what)
  (lets ((r w (popen (format #f "~a ~a ARG 2>/dev/null" *uxnemu-implementation* *tmp-rom-location*)))
         (res (bytes->string (force-ll (port->byte-stream r)))))
    (if (equal? what res)
        1
        0)))

(define (compile-and-expect filename vs)
  (let ((what (get vs 'output 'bug)))
    (if (= 1 (compile filename vs))
        (+ 1 (expect what))
        1)))

;; filename → (values filename possible-tests passed-tests)
(define (run-test filename)
  (lets ((data (assoc '_declare-test (N/file->sexps filename)))
         (vs (if data (output-exp->ff data) empty)))
    (if (has? (keys vs) 'output)
        (values 2 (compile-and-expect filename vs))
        (values 1 (compile filename vs)))))

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
