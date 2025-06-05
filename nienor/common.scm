(define-library (nienor common)
  (import
   (owl toplevel)
   (owl sexp))

  (export
   file->sexps
   warn
   error)

  (begin
    (define (warn . l)
      (print-to stderr "warning: " l))

    (define (error . l)
      (print-to stderr "error: " l)
      (halt 42))

    (define (file->sexps filename)
      (list->sexps (file->list filename) (λ _ _) "syntax error:"))

    ))
