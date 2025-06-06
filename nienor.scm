(import
 (owl toplevel)
 (owl args)
 (owl metric)
 (owl format)
 (nienor common)
 (prefix (nienor dis) d/)
 (prefix (nienor compile) n/))

(define command-line-rules
  (cl-rules
   `((help   "-h" "--help")
     (output "-o" "--output" has-arg comment "target file"        default "a.out")
     (emacro "-m" "--macros"         comment "only expand macros")
     (opt?   "-O" "--optimize"       comment "optimize code")
     (np     "-N" "--no-prelude"     comment "don't attach prelude")
     (dump   "-D" "--dump"           comment "compile & disassemble to uxntal - always attaches prelude and optimizes")
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

     (let* ((out (get opt 'output #f))
            (mac (get opt 'emacro #f))
            (opt? (get opt 'opt? #f))
            (np (get opt 'np #f))
            (att (if np I n/attach-prelude))
            (dump (get opt 'dump #f)))
       (cond
        ((= (length extra) 1)
         (cond
          (mac
           ;; TODO: generalize -o -
           (lets ((_ lst (n/expand-macros (att (file->sexps (car extra))) n/empty-env)))
             (for-each print lst)))
          (dump
           (let ((f (if (equal? out "-")
                        stdout
                        (open-output-file out))))
             (d/disassemble-file (car extra) f)
             (when (not (eq? f stdout))
               (close-port f))))
          (else
           (lets ((env data (n/compile-file (car extra) (if opt? 4 #f) att))
                  (n-labels (ff-fold (λ (a k v) (+ a 1)) 0 (get env 'labels empty))))
             (format stdout "Assembled ~a in ~aB (~,2f% used), ~x labels~%"
                     out
                     (format-number-base2 (len data))
                     (* 100 (/ (len data) (<< 1 16)))
                     n-labels)
             (list->file data out))))
         0)
        ((> (length extra) 1) (error "Too many files."))
        (else
         (error "No input file.")))))))
