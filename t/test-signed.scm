(_declare-test
 output => "-1\n1\n1\n-1\n-1\n1\n1\n-1\n-10\n-6\n6\n-6\n6\n-2\n2\n-2\n2\n")

;; TODO: maybe do some compiler magic type deduction in some far far future

(define (main)
  (print-signed-number (signed+ 10 (negative 11)))
  (print-signed-number (signed+ 10 (negative 9)))
  (print-signed-number (signed+ (negative 10) 11))
  (print-signed-number (signed+ (negative 10) 9))

  (print-signed-number (signed- 10 11))
  (print-signed-number (signed- 10 9))
  (print-signed-number (signed- (negative 10) (negative 11)))
  (print-signed-number (signed- (negative 10) (negative 9)))

  (print-signed-number (signed- 0 10))

  (print-signed-number (signed* (negative 2) 3))
  (print-signed-number (signed* (negative 2) (negative 3)))
  (print-signed-number (signed* 2 (negative 3)))
  (print-signed-number (signed* 2 3))

  (print-signed-number (signed/ (negative 6) 3))
  (print-signed-number (signed/ (negative 6) (negative 3)))
  (print-signed-number (signed/ 6 (negative 3)))
  (print-signed-number (signed/ 6 3))

  (exit 128))
