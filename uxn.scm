(import
 (owl toplevel)
 (prefix (owl sys) sys/))

(define *rom* "/home/kpm/nmojeprogramy/uxn-utils/cli/opctest/bin/opctest.rom")

(define vref vector-ref)

(define (do1 stack f)
  (lets ((head tail stack))
    (cons (f head) tail)))

(define (do2 stack f)
  (lets ((b tail1 stack)
         (a tail2 tail1))
    (append (f a b) tail2)))

(define (error . l)
  (print-to stderr "error: " l)
  (halt 42))

(define (not-implemented name)
  (λ _
    (error (format #f "~a is not implemented" name))))

(define *opcodes*
  (list->vector
   ; name, arglen (in bytes)
   `((brk 0 ,(not-implemented 'brk))
     (inc 1 ,(λ (pc stack) (values pc (do1 stack (λ (a) (+ a 1))))))
     (pop 1 ,(λ (pc stack) (values pc (cdr stack))))
     (nip 2 ,(λ (pc stack) (values pc (do2 stack (λ (a b) (list b))))))
     (swp 2 ,(not-implemented 'swp))
     (rot 3 ,(not-implemented 'rot))
     (dup 1 ,(not-implemented 'dup))
     (ovr 2 ,(not-implemented 'ovr))
     (equ 2 ,(not-implemented 'equ))
     (neq 2 ,(not-implemented 'neq))
     (gth 2 ,(not-implemented 'gth))
     (lth 2 ,(not-implemented 'lth))
     (jmp 1 ,(not-implemented 'jmp))
     (jcn 2 ,(not-implemented 'jcn))
     (jsr 1 ,(not-implemented 'jsr))
     (sth 1 ,(not-implemented 'sth))
     (ldz 1 ,(not-implemented 'ldz))
     (stz 2 ,(not-implemented 'stz))
     (ldr 1 ,(not-implemented 'ldr))
     (str 2 ,(not-implemented 'str))
     (lda 2 ,(not-implemented 'lda))
     (sta 3 ,(not-implemented 'sta))
     (dei 1 ,(not-implemented 'dei))
     (deo 2 ,(not-implemented 'deo))
     (add 2 ,(not-implemented 'add))
     (sub 2 ,(not-implemented 'sub))
     (mul 2 ,(not-implemented 'mul))
     (div 2 ,(not-implemented 'div))
     (and 2 ,(not-implemented 'and))
     (ora 2 ,(not-implemented 'ora))
     (eor 2 ,(not-implemented 'eor))
     (sft 2 ,(not-implemented 'sft))
     (jci 1 ,(not-implemented 'jci))
     (jmi 0 ,(not-implemented 'jmi))
     (jsi 0 ,(not-implemented 'jsi))
     (lit 0 ,(not-implemented 'lit))
     )))

(define *opcode-names* (vector-map car *opcodes*))

(define (short-mode? op)
  (= (band #b00100000 op) #b00100000))

(define (return-mode? op)
  (= (band #b01000000 op) #b01000000))

(define (keep-mode? op)
  (= (band #b10000000 op) #b10000000))

(define (cdxr l n)
  (if (= n 0)
      l
      (cdxr (cdr l) (- n 1))))

(define (opcode-of op)
  (cond
   ((= (band op #b10000000) #b10000000) (vref *opcodes* 35))
   ((= op #x20) (vref *opcodes* 32))
   ((= op #x40) (vref *opcodes* 33))
   ((= op #x60) (vref *opcodes* 34))
   (else
    (vref *opcodes* (band #b00011111 op)))))

(define (dis lst)
  (if (null? lst)
      #t
      (lets ((opn rest lst)
             (op (opcode-of opn))
             (lit? (eq? 'lit (car op)))
             (short? (short-mode? opn)))
        (cond
         ((and lit? short?) (format stdout "~a (~a)~%" (car op) (bior (<< (car rest) 8) (cadr rest))))
         (lit? (format stdout "~a (~a)~%" (car op) (car rest)))
         (else
          (format stdout "~a~%" (car op))))

        (if lit?
            (if short?
                (dis (cddr rest))
                (dis (cdr rest)))
            (dis rest))
        )))

;; what if memory was lazily-evaluated

;; (define (emu lst)
;;   (let loop ((mem lst) (stack #n))
;;     (lets ((opn rest lst)
;;            (op (opcode-of opn))
;;            (lit? (eq? 'lit (car op)))
;;            (short? (short-mode? opn)))
;;       (cond
;;        ((and lit? short?) (format stdout "~a (~a)~%" (car op) (bior (<< (car rest) 8) (cadr rest))))
;;        (lit? (format stdout "~a (~a)~%" (car op) (car rest)))
;;        (else
;;         (format stdout "~a~%" (car op))))

;;       (if lit?
;;           (if short?
;;               (dis (cddr rest))
;;               (dis (cdr rest)))
;;           (dis rest))
;;       )))
