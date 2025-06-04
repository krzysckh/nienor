(import
 (owl toplevel)
 (owl sexp))

(define-values (BRK INC POP NIP SWP ROT
                DUP OVR EQU NEQ GTH LTH
                JMP JCN JSR STH LDZ STZ
                LDR STR LDA STA DEI DEO
                ADD SUB MUL DIV AND ORA
                EOR SFT JCI JMI JSI LIT)
  (values
   #x00 #x01 #x02 #x03 #x04 #x05 #x06
   #x07 #x08 #x09 #x0a #x0b #x0c #x0d
   #x0e #x0f #x10 #x11 #x12 #x13 #x14
   #x15 #x16 #x17 #x18 #x19 #x1a #x1b
   #x1c #x1d #x1e #x1f #x20 #x40 #x60
   #x80))

(define *opcodes*
  (pipe empty
    (put 'brk `(,BRK 0))
    (put 'inc `(,INC 1))
    (put 'pop `(,POP 1))
    (put 'nip `(,NIP 2))
    (put 'swp `(,SWP 2))
    (put 'rot `(,ROT 3))
    (put 'dup `(,DUP 1))
    (put 'ovr `(,OVR 2))
    (put 'equ `(,EQU 2))
    (put 'neq `(,NEQ 2))
    (put 'gth `(,GTH 2))
    (put 'lth `(,LTH 2))
    (put 'jmp `(,JMP 1))
    (put 'jcn `(,JCN 2))
    (put 'jsr `(,JSR 1))
    (put 'sth `(,STH 1))
    (put 'ldz `(,LDZ 1))
    (put 'stz `(,STZ 2))
    (put 'ldr `(,LDR 1))
    (put 'str `(,STR 2))
    (put 'lda `(,LDA 2))
    (put 'sta `(,STA 3))
    (put 'dei `(,DEI 1))
    (put 'deo `(,DEO 2))
    (put 'add `(,ADD 2))
    (put 'sub `(,SUB 2))
    (put 'mul `(,MUL 2))
    (put 'div `(,DIV 2))
    (put 'and `(,AND 2))
    (put 'ora `(,ORA 2))
    (put 'eor `(,EOR 2))
    (put 'sft `(,SFT 2))
    (put 'jci `(,JCI 1))
    (put 'jmi `(,JMI 0))
    (put 'jsi `(,JSI 0))
    (put 'lit `(,LIT 0))))

;; (define *code*
;;   '((with-codegen-at! #x100
;;       (lit! 38)
;;       (lit! 12)
;;       (eor))))

(define (error . l)
  (print-to stderr "error: " l)
  (halt 42))

(define (bior* . l) (fold bior 0 l))

(define (opcode opc short? return? keep?)
  (let ((op (get *opcodes* opc #f)))
    (if op
        (tuple 'bytes (list (bior*
                             (car op)
                             (if short?  #b001 0)
                             (if return? #b01 0)
                             (if keep?   #b1 0))))
        (error "unknown opcode: " opc))))

(define (codegen at lst)
  (print "lst: " lst)
  `(,at
    . ,(λ (env) ; env -> (values code env')
         (let loop ((lst lst)
                    (at at)
                    (env env)
                    (acc #n))
           (if (null? lst)
               (values
                acc
                env)
               (lets ((exp rest lst))
                 (when (not (list? exp))
                   (error exp " is not a valid expression: (not (list? ...))"))
                 (tuple-case (list->tuple exp)
                   ((define-label! label)
                    (loop (cdr lst)
                          at
                          (put env 'labels (put (get env 'labels empty) label at))
                          acc))
                   ((uxn-call! mode opc)
                    (let ((short?  (has? mode 2))
                          (return? (has? mode 'r))
                          (keep?   (has? mode 'k)))
                      (loop (cdr lst) (+ at 1) env (append acc (list (opcode opc short? return? keep?))))))
                   (else
                    (error "unknown expression for codegen: " exp)
                    ))))))))

(define (compile lst)
  (let loop ((lst lst)
             (acc #n))
    (if (null? lst)
        acc
        (let ((exp (car lst)))
          (tuple-case (list->tuple exp)
            ((with-codegen-at! ptr code)
             (loop (cdr lst) (append acc (list (codegen ptr code)))))
            (else
             (error "unknown raw compiler directive " (car lst))))))))

(define empty-env
  (pipe empty
    (put 'labels empty) ; ff, global
    (put 'locals #n)    ; a list, newest consed before, then removed at free-locals!
    ))

(define compiled (compile (list->sexps (file->list "test.l") (λ _ _) "syntax error:")))
(print compiled)

(define fully-compiled
  (fold (λ (data c)
          (lets ((env code data)
                 (at f c)
                 (code* env* (f env)))
            (print "code: " code)
            (print "env: " (ff->list env*))
            (cons env* (append code (list code*)))))
        (cons empty-env #n)
        compiled))

(print fully-compiled)
