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
     (dump   "-D" "--dump"           comment "compile & disassemble to uxntal")
     (V      "-V" "--verbose"        comment "be verbose")
     (q      "-q" "--quiet"          comment "be quiet")
     )))

(define (print-used-labels env)
  (print "Used labels:")
  (ff-map
   (λ (k v) (format stdout "  ~a: |~4,'0x~%" k v))
   (get env 'labels empty)))

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
            (v (get opt 'V #f))
            (dump (get opt 'dump #f)))
       (cond
        ((= (length extra) 1)
         (cond
          (mac
           ;; TODO: generalize -o -
           (for-each
            print
            (call/cc (λ (c) (n/compile (n/attach-prelude (file->sexps (car extra))) (if opt? 4 #f) #f c #f)))))
          (dump
           (let ((f (if (equal? out "-")
                        stdout
                        (open-output-file out))))
             (d/disassemble-file (car extra) f opt?)
             (when (not (eq? f stdout))
               (close-port f))))
          (else
           (lets ((env data (n/compile-file (car extra) opt? v))
                  (n-labels (ff-fold (λ (a k v) (+ a 1)) 0 (get env 'labels empty))))
             (unless (get opt 'q #f)
               (format stdout "Assembled ~a in ~aB (~,2f% used), ~a labels~%"
                       out
                       (format-number-base2 (len data))
                       (* 100 (/ (len data) (<< 1 16)))
                       n-labels))
             ;; TODO: delete unused labels so i can show them here
             ;; (when v
             ;;   (print-used-labels env))
             (list->file data out))))
         0)
        ((> (length extra) 1) (error "Too many files."))
        (else
         (error "No input file.")))))))
