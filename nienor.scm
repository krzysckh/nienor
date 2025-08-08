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
     (output "-o" "--output"            has-arg comment "target file"               default "a.out")
     (emacro "-m" "--expand-macros"             comment "only expand and dump macros")
     (M      "-M" "--macroexpand"       has-arg comment "expand one macro (the argument) and exit")
     (dump   "-D" "--dump"                      comment "compile & disassemble to uxntal")
     (V      "-V" "--verbose"                   comment "be verbose")
     (q      "-q" "--quiet"                     comment "be quiet")
     (s      "-s" "--dump-symbol-table"         comment "dump symbol table to file")
     (sf     "-S" "--symbol-table-file" has-arg comment "set symbol table filename" default "a.out.sym")
     )))

(define (print-used-labels env)
  (print "Used labels:")
  (ff-map
   (λ (k v) (format stdout "  ~a: |~4,'0x~%" k v))
   (get env 'labels empty)))

;; TODO: constants and labels are stored in the same ff so this list is a bit wrong
(define (dump-symbol-table env out)
  (list->file
   (ff-fold
    (λ (a k v)
      (append
       a
       (list (>> (band #xff00 v) 8) (band #xff v))
       (string->bytes (symbol->string k))
       '(0)))
    '()
    (get env 'labels empty))
   out))

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
            (np (get opt 'np #f))
            (verbose? (get opt 'V #f))
            (symt (get opt 's #f))
            (quiet? (get opt 'q #f))
            (macroexpand? (get opt 'M #f))
            (dump (get opt 'dump #f)))
       (when (and quiet? verbose?)
         (error "Cannot specify both --quiet and --verbose. You lose."))
       (cond
        (macroexpand?
         (lets ((_ env (call/cc2 (λ (c) (n/compile (n/attach-prelude ()) #f c verbose?)))))
           (lets ((_ l (n/expand-macros `((flatten! ,@(string->sexps macroexpand?))) env))
                  (l env (call/cc2 (λ (c) (n/compile l #f c verbose? env)))))
             (for-each
              (λ (e)
                (let ((s (str* e)))
                  (print
                   (if (eq? (string-ref s 0) #\')
                       (substring s 1 (string-length s))
                       s))))
              (if (list? l) l (list l)))
             0)))
        ((= (length extra) 1)
         (cond
          (mac
           ;; TODO: generalize -o -
           (lets ((l _ (call/cc2 (λ (c) (n/compile (n/attach-prelude (file->sexps (car extra))) #f c verbose?)))))
             (for-each print l)))
          (dump
           (let ((f (if (equal? out "-")
                        stdout
                        (open-output-file out))))
             (d/disassemble-file (car extra) f)
             (when (not (eq? f stdout))
               (close-port f))))
          (else
           (lets ((env data (n/compile-file (car extra) verbose?))
                  (n-labels (ff-fold (λ (a k v) (+ a 1)) 0 (get env 'labels empty))))
             (unless quiet?
               (format stdout "Assembled ~a in ~aB (~,2f% used), ~a labels~%"
                       out
                       (format-number-base2 (len data))
                       (* 100 (/ (len data) (<< 1 16)))
                       n-labels))
             ;; TODO: delete unused labels so i can show them here
             ;; (when v
             ;;   (print-used-labels env))
             (list->file data out)
             (when symt
               (dump-symbol-table env (get opt 'sf 'bug)))
             )))
         0)
        ((> (length extra) 1) (error "Too many files."))
        (else
         (error "No input file.")))))))
