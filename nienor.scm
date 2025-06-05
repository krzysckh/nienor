(import
 (owl toplevel)
 (owl args)
 (owl metric)
 (nienor common)
 (prefix (nienor compile) n/))

(define command-line-rules
  (cl-rules
   `((help   "-h" "--help")
     (output "-o" "--output" has-arg comment "target file" default "a.out")
     ;; (ast    "-p" "--print-ast" comment "print the AST")
     )))

(λ (args)
  (process-arguments
   (cdr args) command-line-rules "you lose"
   (λ (opt extra)
     (when (get opt 'help #f)
       (print "Usage: " (car args) " [args] [file]")
       (print-rules command-line-rules)
       (halt 0))

     (let ((out (get opt 'output #f)))
       (cond
        ((= (length extra) 1)
         (let ((data (n/compile-file (car extra))))
           (print "Assembled to " (format-number-base2 (len data)) "B")
           (list->file data out)
           0))
        ((> (length extra) 1) (error "Too many files."))
        (else
         (error "No input file.")))))))
