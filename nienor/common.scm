(define-library (nienor common)
  (import
   (owl toplevel)
   (owl eval)
   (owl metric)
   (owl sexp))

  (export
   file->sexps
   string->sexps
   empty-env
   warn
   error
   add-macros
   flatten

   verbose?
   imm?
   apply*

   with-timer
   lets/timer

   ;; pp

   maybe-opc
   short!  short?
   return! return?
   keep!   keep?

   *opcodes*
   BRK INC POP NIP SWP ROT
   DUP OVR EQU NEQ GTH LTH
   JMP JCN JSR STH LDZ STZ
   LDR STR LDA STA DEI DEO
   ADD SUB MUL DIV AND ORA
   EOR SFT JCI JMI JSI LIT
   )

  (begin
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

    ;; TODO: some simpler opcode calling, actually use the arity i store here
    ;; TODO: opcodes should be called as if they were normal functions. less magic.
    ;;       (begin place (uxn-call! (2) jmp)) -> (__jmp place)
    ;;       just get rid of uxn-call!, it's so ugly, and the (2 k r) doesn't make any syntactical sense
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

    (define empty-env
      (pipe empty
        (put 'labels   empty) ; ff of labels & constants, global
        (put 'vars     #n)    ; list of variables. these are labels that need to have _get! appended to them for easier usage
        (put 'locals   #n)    ; a list, newest consed before, then removed at free-locals!
        (put 'macros   empty) ; ff of macro-name -> λ (exp) -> rewritten
        (put 'epilogue #n)    ; epilogue = code compiled later
        (put 'arity    empty) ; ff of defun-name -> arity for simple arity checking ;; <- TODO: move this somewhere else
        (put 'symbols  empty) ; ff of symbol-name -> id
        (put 'verbose? #f)    ; self explanatory
        (put 'tcheck   empty) ; ff of function-name -> ff of args=(T ...) result=T' inferred=#t|#f
        (put 'tcheckd  empty) ; ff of function-name -> list of ((T ...) ...)
        (put 'trules   empty) ; ff of t1 -> ff of t2 -> λx -> #t | #f
        (put '_compile #f)    ; compile function itself (what an ugly hack!) ; TODO: <- wow, this is a hack
        ))

    (define imm? (B not pair?))

    (define (maybe-opc x) (if (symbol? x) (get *opcodes* x #f) x))
    (define (short! x)  (bior (maybe-opc x) #b00100000))
    (define (return! x) (bior (maybe-opc x) #b01000000))
    (define (keep! x)   (bior (maybe-opc x) #b10000000))

    (define (short? op)  (= (band #b00100000 op) #b00100000))
    (define (return? op) (= (band #b01000000 op) #b01000000))
    (define (keep? op)   (= (band #b10000000 op) #b10000000))

    (define (err type mesg rest)
      (print-to stderr (str type) ": " mesg)
      (when rest
        (for-each
         (H print-to stderr)
         (map (λ (s) (str (make-string (+ 2 (string-length (str type))) #\space) s)) rest))))

    (define (warn mesg . l)
      (err 'warning mesg l))

    (define (error mesg . l)
      (err 'error mesg l)
      (halt 42))

    (define (error* mesg . l)
      (err 'error mesg l))

    (define (file->sexps filename)
      (list->sexps (file->list filename) (λ _ _) "syntax error:"))

    (define (string->sexps s)
      (list->sexps (string->bytes s) (λ _ _) "syntax error:"))

    ;; where app = add-macro or alike
    (define (add-macros app lst env)
      (fold (λ (a b) (app a (cadr b) (caddr b) (car b))) env lst))

    (define (verbose? env)
      (get env 'verbose? #f))

    (define-syntax with-timer
      (syntax-rules ()
        ((_ (arg ...) . body)
         (let ((t0 (time-ns)))
           (lets ((arg ... (begin . body)))
             (format stderr "    [timer] ~a took ~a~%" 'body (format-time (- (time-ns) t0)))
             (values arg ...))))))

    (define-syntax lets/timer
      (syntax-rules ()
        ((_ verbose? ((arg ... exp) ...) . body)
         (lets ((arg ... (if verbose? (with-timer (arg ...) exp) exp)) ...)
           . body))))

    (define (apply* f l)
      (if (> (len l) 5)
          (exported-eval `(,f ,@l) *toplevel*)
          (apply f l)))

    (define (flatten l)
      (cond
       ((null? l) #n)
       ((pair? (car* l)) (append (flatten (car l)) (flatten (cdr l))))
       ((pair? l) (cons (flatten (car l)) (flatten (cdr l))))
       (else l)))

    ))
