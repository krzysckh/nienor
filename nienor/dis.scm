(define-library (nienor dis)
  (import
   (owl toplevel)
   (owl format)
   (nienor common)
   (prefix (nienor compile) n/))

  (export
   disassemble
   disassemble-file)

  (begin
    (define *opc*
      (fold
       (λ (a b)
         (put a (car (get *opcodes* b #f)) b))
       empty
       (keys *opcodes*)))

    (define (unlist l)
      (fold (λ (a b) (str a b " ")) "" (map str l)))

    (define (to-uppercase s)
      (string-map (λ (x) (if (>= x 97) (- x 32) x)) (str s)))

    (define (comment code)
      (let ((o (car code)))
        (cond
         ((= o JCI) 'JCI)
         ((= o JMI) 'JMI)
         ((= o JSI) 'JSI)
         (else
          (format
           #f
           "~a~a~a~a"
           (to-uppercase (get *opc* (band (car code) #b00011111) 'unknown))
           (if (short? (car code)) "2" "")
           (if (return? (car code)) "r" "")
           (if (keep? (car code)) "k" ""))))))

    (define (disassemble code out)
      (let loop ((code code) (at #x100))
        (cond
         ((null? code) #t)
         ((list? (car code))
          (format out "( ~a)~%" (unlist (car code)))
          (loop (cdr code) at))
         ((or (= (car code) #xa0) (= (car code) #xe0))
          (format out "|~4,'0x   ~2,'0x ~2,'0x ~2,'0x    (  )~%" at (car code) (cadr code) (caddr code))
          (loop (cdddr code) (+ at 3)))
         ((or (= (car code) #x80) (= (car code) #xc0))
          (format out "|~4,'0x   ~2,'0x ~2,'0x       ( )~%" at (car code) (cadr code))
          (loop (cddr code) (+ at 2)))
         (else
          (format out "|~4,'0x   ~2,'0x          ( ~a )~%" at (car code) (comment code))
          (loop (cdr code) (+ at 1))))))

    (define (disassemble-file filename out disT?)
      (lets ((_ data (n/compile (n/attach-prelude (file->sexps filename)) #t #f #f disT? #f)))
        (disassemble data out)))

    ))
