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
                             (if short?  #b00100000 0)
                             (if return? #b01000000 0)
                             (if keep?   #b10000000 0))))
        (error "unknown opcode: " opc))))

(define (maybe-opc x) (if (symbol? x) (get *opcodes* x #f) x))
(define (short! x)  (bior (maybe-opc x) #b00100000))
(define (return! x) (bior (maybe-opc x) #b01000000))
(define (keep! x)   (bior (maybe-opc x) #b10000000))

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
                at
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
                   ((push! value)
                    (lets ((at code
                               (cond
                                ((number? value)
                                 (if (< value 256)
                                     (values (+ at 2) (list (tuple 'bytes `(,LIT ,value))))
                                     `(error "shorts not supported yet: " value)))
                                ((symbol? value)
                                 (let ((n (let loop ((l (get env 'locals #n)))
                                            (when (null? l)
                                              (error "local " value " not found"))
                                            (if (eq? (car l) value)
                                                0
                                                (+ 1 (loop (cdr l)))))))
                                   (print "will get local " value " from ptr - " n)
                                   (values (+ at 7) (list (tuple 'bytes `(,LIT 0 ,LDZ ,LIT ,n ,SUB ,LDZ))))))
                                ((list? value)
                                 (lets ((dat (codegen at (list value)))
                                        (_ f dat)
                                        (_ (print "at was: " at))
                                        (at code _ (f env)))
                                   (print "code: " code)
                                   (print "at is: " at)
                                   (values at code)))
                                (else
                                 (error "unsupported type for push!: " value)))))
                      (loop (cdr lst) at env (append acc code))))
                   ((free-locals! n)
                    (let ((code `(,LIT 0 ,LDZ  ; load local ptr
                                  ,LIT ,n ,SUB ; subtract the amount of freed locals
                                  ,LIT 0 ,STZ  ; save new local ptr to 0x0
                                  )))
                      (loop (cdr lst)
                            (+ at (len code))
                            (put env 'locals (let loop ((lst (get env 'locals #n)) (n n))
                                               (if (= n 0)
                                                   lst
                                                   (loop (cdr lst) (- n 1)))))
                            (append acc (list (tuple 'bytes code))))))
                   ((allocate-local! name)
                    (let ((code `(,LIT 0 ,LDZ      ; load ptr
                                  ,INC             ; inc ptr
                                  ,STZ             ; store arg at ptr
                                  ,LIT 0 ,LDZ ,INC ; load & inc ptr again (TODO: this might be optimizable)
                                  ,LIT 0 ,STZ      ; store new ptr at 0x0
                                  )))
                      (loop
                       (cdr lst)
                       (+ at (len code))
                       (put env 'locals (cons name (get env 'locals #n)))
                       (append acc (list (tuple 'bytes code))))))
                   ((defun name args body)
                    (lets ((dat (codegen
                                 at
                                 `((define-label! ,name)
                                   ,@(map (λ (s) `(allocate-local! ,s)) args) ; bump local counter in zero page, arg to zero-page
                                   ,@body
                                   (free-locals! ,(len args))
                                   (uxn-call! (2 r) jmp))))
                           (_ f dat)
                           (at code env* (f env)))
                      (loop (cdr lst) at env* (append acc code))))
                   ((uxn-call! mode opc)
                    (let ((short?  (has? mode 2))
                          (return? (has? mode 'r))
                          (keep?   (has? mode 'k)))
                      (loop (cdr lst) (+ at 1) env (append acc (list (opcode opc short? return? keep?))))))
                   ((funcall! func)
                    (let ((resolve (λ (loc) `(,LIT ,(>> (band #xff00 loc) 8) ,LIT ,(band #xff loc) ,(short! JSR)))))
                      ;; TODO: remove magic length
                      (if-lets ((loc (get (get env 'labels empty) func #f)))
                        (loop (cdr lst) (+ at 5) env (append acc (list (tuple 'bytes (resolve loc))))) ; great! we have loc rn, we can resolve
                        (loop (cdr lst) (+ at 5) env (append acc (list (tuple 'unresolved-symbol func resolve))))))) ; we do it later
                   (else
                    (lets ((func (car* exp))
                           (args (cdr* exp))
                           (dat (codegen at `(,@(map (λ (a) `(push! ,a)) args) (funcall! ,func))))
                           (_ f dat)
                           (at code env* (f env)))
                      (loop (cdr lst) at env* (append acc code))))
                   )))))))

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
                 (_ code* env* (f env)))
            (print "code: " code)
            (print "env: " (ff->list env*))
            (cons env* (append code (list code*)))))
        (cons empty-env #n)
        compiled))

(print fully-compiled)

(define (finalize env lst)
  (fold (λ (a b) (tuple-case b
                   ((bytes l)
                    (append a l))
                   ((unresolved-symbol symbol resolve)
                    (if-lets ((loc (get (get env 'labels empty) symbol)))
                      (let ()
                        (print "found " symbol " at " loc)
                        (append a (resolve loc)))
                      (error "couldn't resolve symbol " symbol)))
                   (else
                    (error "unknown directive " b))))
        #n
        lst))

(list->file (finalize (car fully-compiled) (cadr fully-compiled)) "temp.bin")
