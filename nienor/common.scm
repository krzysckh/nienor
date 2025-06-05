(define-library (nienor common)
  (import
   (owl toplevel))

  (export
   error)

  (begin
    (define (error . l)
      (print-to stderr "error: " l)
      (halt 42))
    ))
