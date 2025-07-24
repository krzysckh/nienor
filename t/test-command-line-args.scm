(_declare-test
 output => "args:\n- ARG\n")

(define (main)
  (when (has-command-line-arguments?)
    (call-with-command-line
     (λ (args)
       (print "args:\n")
       (for-each
        (λ (s)
          (print "- ")
          (print s)
          (putchar #\newline))
        args))))
  (exit!))
