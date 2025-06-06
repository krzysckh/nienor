(define-library (nienor compile)
  (import
   (owl toplevel)
   (nienor common)
   (nienor macro))

  (export
   empty-env
   attach-prelude
   expand-macros
   compile
   compile-file)

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

    (define (gensym)
      (interact 'gensym '_))

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

    (define (make-defun codegen cont env acc rest at name args body mode)
      (lets ((dat (codegen
                   at
                   `((define-label! ,name)
                     ,@(map (λ (s) `(allocate-local! ,s)) args) ; bump local counter in zero page, arg to zero-page
                     ,@body
                     (free-locals! ,(len args))
                     ,@(if (eq? mode 'vector)
                           '((uxn-call! () brk)) ; a list because i had something something in mind
                           '((uxn-call! (2 r) jmp)))
                     )))
             (_ f dat)
             (at code env* (f env)))
        (cont rest at env* (append acc code))))

    (define (add-label env name value . nounused)
      (let ((labels (get env 'labels empty))
            (unused (get env 'unused empty)))
        (lets ((env (put env 'labels (put labels name value))))
          (put
           env
           'unused
           (if (null? nounused)
               (if (get unused name #t)
                   (put unused name #t)
                   unused)
               unused)))))

    (define (remove-unused env name)
      (put env 'unused (put (get env 'unused empty) name #f)))

    (define (codegen at lst)
      ;; (print "lst: " lst)
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
                     (if (list? exp)
                         (tuple-case (list->tuple exp)
                           ((define-label! label)
                            (loop rest
                                  at
                                  (add-label env label at)
                                  acc))
                           ((define-constant name value)
                            (loop rest
                                  at
                                  (add-label env name value 'nah)
                                  acc))
                           ((nalloc! name n-bytes)
                            (loop rest
                                  (+ at n-bytes)
                                  (add-label env name at)
                                  (append acc (list (tuple 'bytes (make-list n-bytes 0))))))
                           ((_alloc! name bytes)
                            (let ((bytes (fold
                                          (λ (a b)
                                            (append
                                             a
                                             (cond
                                              ((string? b) (string->bytes b))
                                              ((symbol? b) (error "_alloc! with variables is not implemented"))
                                              (else (list b)))))
                                          #n
                                          bytes)))
                              (loop rest
                                    (+ at (len bytes))
                                    (add-label env name at)
                                    (append acc (list (tuple 'bytes bytes))))))
                           ((codegen-at! ptr)
                            (loop rest ptr env (append acc (list (tuple 'codegen-at ptr)))))
                           ((_push! mode value)
                            (lets ((byte? (eq? mode 'byte))
                                   (env at code
                                       (cond
                                        ((number? value)
                                         (if byte?
                                             (values env (+ at 2) (list (tuple 'bytes `(,LIT ,(band #xff value)))))
                                             (values env (+ at 4) (list (tuple 'bytes `(,LIT ,(>> (band #xff00 value) 8) ,LIT ,(band #xff value)))))))
                                        ((eq? value #t)
                                         (if byte?
                                             (values env (+ at 2) (list (tuple 'bytes `(,LIT 1))))
                                             (values env (+ at 4) (list (tuple 'bytes `(,LIT 0 ,LIT 1))))))
                                        ((eq? value #f)
                                         (if byte?
                                             (values env (+ at 2) (list (tuple 'bytes `(,LIT 0))))
                                             (values env (+ at 4) (list (tuple 'bytes `(,LIT 0 ,LIT 0))))))
                                        ((symbol? value)
                                         (let loop ((l (get env 'locals #n)) (n 0))
                                           (cond
                                            ((null? l)
                                             ;; did not found value in locals, try searching in function labels
                                             (let ((resolve (λ (loc)
                                                              (if byte?
                                                                  `(,LIT ,(band #xff loc))
                                                                  `(,LIT ,(>> (band #xff00 loc) 8) ,LIT ,(band #xff loc))))))
                                               ;; TODO: remove magic length, dry with funcall!
                                               (values
                                                (remove-unused env value)
                                                (+ at (if byte? 2 4))
                                                (if-lets ((loc (get (get env 'labels empty) value #f)))
                                                  (list (tuple 'bytes (resolve loc))) ; great! we have loc rn, we can resolve
                                                  (list (tuple 'unresolved-symbol value resolve)))))) ; we do it later
                                            ((eq? (car l) value)
                                             (values
                                              env
                                              (+ at (if byte? 11 10))
                                              (list (tuple 'bytes `(,LIT 0 ,LDZ ,LIT ,n ,SUB ,LIT 2 ,MUL ,(short! LDZ) ,@(if byte? `(,NIP) #n))))))
                                            (else
                                             (loop (cdr l) (+ n 1))))))
                                        ((list? value)
                                         (lets ((dat (codegen at (list value)))
                                                (_ f dat)
                                                (at code env (f env)))
                                           (values env at code)))
                                        ((string? value)
                                         (let ((raw (fold append #n (map (λ (x) `(,LIT 0 ,LIT ,x)) (reverse (string->bytes value))))))
                                           (values env (len raw) (list (tuple 'bytes raw)))))
                                        (else
                                         (error "unsupported type for _push!: " value)))))
                              (loop rest at env (append acc code))))
                           ((free-locals! n)
                            (let ((code `(,LIT 0 ,LDZ  ; load local ptr from 0x0
                                          ,LIT ,n ,SUB ; subtract the amount of freed locals
                                          ,LIT 0 ,STZ  ; save new local ptr to 0x0
                                          )))
                              (loop rest
                                    (+ at (len code))
                                    (put env 'locals (let loop ((lst (get env 'locals #n)) (n n))
                                                       (if (= n 0)
                                                           lst
                                                           (loop rest (- n 1)))))
                                    (append acc (list (tuple 'bytes code))))))
                           ((allocate-local! name)
                            (let ((code `(,LIT 0 ,LDZ      ; load ptr
                                          ,INC             ; inc ptr
                                          ,LIT 2 ,MUL      ; *2 because all locals are shorts
                                          ,(short! STZ)    ; store arg at ptr
                                          ,LIT 0 ,LDZ ,INC ; load & inc ptr again
                                          ,LIT 0 ,STZ      ; store new ptr at 0x0
                                          )))
                              (loop
                               rest
                               (+ at (len code))
                               (put env 'locals (cons name (get env 'locals #n)))
                               (append acc (list (tuple 'bytes code))))))
                           ((_defun name args body)
                            (make-defun codegen loop env acc rest at name args body 'normal)) ; continues loop
                           ((_defun-vector name args body)
                            (make-defun codegen loop env acc rest at name args body 'vector)) ; continues loop
                           ((uxn-call! mode opc)
                            (let ((short?  (has? mode 2))
                                  (return? (has? mode 'r))
                                  (keep?   (has? mode 'k)))
                              (loop rest (+ at 1) env (append acc (list (opcode opc short? return? keep?))))))
                           ((funcall! func)
                            (let ((resolve (λ (loc) `(,LIT ,(>> (band #xff00 loc) 8) ,LIT ,(band #xff loc) ,(short! JSR)))))
                              ;; TODO: remove magic length
                              (if-lets ((loc (get (get env 'labels empty) func #f)))
                                (loop rest (+ at 5) (remove-unused env func) (append acc (list (tuple 'bytes (resolve loc))))) ; great! we have loc rn, we can resolve
                                (loop rest (+ at 5) (remove-unused env func) (append acc (list (tuple 'unresolved-symbol func resolve))))))) ; we do it later
                           ((if arg then else)
                            (lets ((func (car* exp))
                                   (args (cdr* exp))
                                   (end (gensym))
                                   (then-loc (gensym))
                                   (dat (codegen at `(,arg
                                                      (_push! _ ,then-loc)
                                                      (uxn-call! (2) jcn)
                                                      ,else
                                                      (_push! _ ,end)
                                                      (uxn-call! (2) jmp)
                                                      (define-label! ,then-loc)
                                                      ,then
                                                      (define-label! ,end))))
                                   (_ f dat)
                                   (at code env* (f env)))
                              (loop rest at env* (append acc code))))
                           ((_with-label label body)
                            (lets ((label-was (get (get env 'labels) label #f))
                                   (env (add-label env label at))
                                   (dat (codegen at body))
                                   (_ f dat)
                                   (at code env* (f env)))
                              (loop rest
                                    at
                                    (if label-was
                                        (put env* 'labels (put (get env* 'labels empty) label label-was))
                                        (put env* 'labels (del (get env* 'labels empty) label)))
                                    (append acc code))))
                           (else ; funcall
                            (lets ((func (car* exp))
                                   (args (cdr* exp))
                                   (dat (codegen at `(,@(map (λ (a) `(_push! _ ,a)) (reverse args)) (funcall! ,func))))
                                   (_ f dat)
                                   (at code env* (f env)))
                              (loop rest at env* (append acc code)))))
                         (loop `((_push! _ ,exp) ,@rest) at env acc) ; if we got an atom, just push it
                         )))))))

    (define empty-env
      (pipe empty
        (put 'labels empty) ; ff of labels & constants, global
        (put 'locals #n)    ; a list, newest consed before, then removed at free-locals!
        (put 'macros empty) ; ff of macro-name -> λ (exp) -> rewritten
        (put 'unused empty) ; ff of label -> truthy value when unused
        ))

    ;; env rule rewrite → env'
    (define (add-macro env rule rewrite literal)
      (let* ((base (λ (exp) (error "couldn't match" exp "to any rewrite rules")))
             (name (car rule))
             (was (get (get env 'macros empty) name base)))
        (put
         env
         'macros
         (put
          (get env 'macros empty)
          name
          (λ (exp)
            (if (macro-matches? rule exp literal)
                (rewrite-macro rule rewrite exp literal)
                (was exp)))))))

    (define (apply-macros env exp)
      (let ((macros (get env 'macros empty)))
        (let walk ((exp exp))
          (cond
           ((null? exp) #n)
           ((get macros (car* exp) #f)
            (walk ((get macros (car* exp) #f) exp)))
           ((pair? (car exp)) (cons
                               (walk (car exp))
                               (walk (cdr exp))))
           (else
            (cons (car exp) (walk (cdr exp))))))))

    ;; env exp → env' exp'
    (define (lookup-toplevel-macros env exp)
      (let loop ((exp exp) (env env) (acc #n) (substitutions 0))
        (if (null? exp)
            (values substitutions env acc)
            (tuple-case (list->tuple (car* exp))
              ((define-macro-rule literal rule rewrite)
               (loop (cdr exp) (add-macro env rule rewrite literal) acc (+ substitutions 1)))
              (else
               (loop (cdr exp) env (append acc (list (car exp))) substitutions))))))

    (define (resolve env lst)
      (fold (λ (a b)
              (tuple-case b
                ((codegen-at ptr*)
                 (let ((ptr (- ptr* #x100)))
                   (if (> (len a) ptr)
                       (error "attempting to codegen-at! on code that was already written " ptr)
                       (append a (make-list (- ptr (len a)) 0)))))
                ((bytes l)
                 (append a l))
                ((unresolved-symbol symbol resolve)
                 (if-lets ((loc (get (get env 'labels empty) symbol)))
                   (append a (resolve loc))
                   (error "couldn't resolve symbol " symbol)))
                (else
                 (error "unknown directive " b))))
            #n
            lst))

    (define (expand-macros lst env)
      (lets ((substitutions env lst (lookup-toplevel-macros env lst)))
        (if (= substitutions 0)
            (values
             env
             (apply-macros env lst))
            (expand-macros (apply-macros env lst) env))))

    (define unused-no-complain '(nigeb))

    (define (get-unused env)
      (filter
       (λ (e) (not (has? unused-no-complain e)))
       (ff-fold
        (λ (a k v) (if v (cons k a) a))
        #n
        (get env 'unused empty))))

    (define (delete-unused-defuns unused lst)
      (filter
       (λ (l)
         (if (eq? (car* l) '_defun)
             (not (has? unused (car* (cdr* l))))
             #t))
       lst))

    (define (compile lst opt?)
      ;; a toplevel thread didn't seem to compile correctly
      (thread
       'gensym
       (let loop ((n 1))
         (lets ((who v (next-mail)))
           (tuple-case v
             ((exit!)
              (mail who 'ok))
             (else
              (mail who (string->symbol (str "@@gensym__" n)))
              ;; was: (mail who (string->symbol (str "g" n)))
              ;; i had a crazy bug where i named a variable g1 & g2 and it freaked out on an if statement
              ;; which - coincidentally - is the only piece of codegen that uses gensyms
              ;; makes you wonder...
              ;; TODO: use temporary labels in if
              (loop (+ n 1)))))))

      (lets ((opt? (cond ; limit passes to 4
                    ((eq? opt? 0) #f)
                    ((eq? opt? #f) #f)
                    ((not (number? opt?)) 4)
                    (else
                     (- opt? 1))))
             (env lst (expand-macros lst empty-env))
             (_ code* env* ((cdr (codegen #x100 lst)) env))) ; <- gensym is only needed here
        (interact 'gensym (tuple 'exit!))                    ; so we can kill it afterwards
        (let ((unused (get-unused env*)))
          (if (and opt? (not (null? unused)))
              (compile (delete-unused-defuns unused lst) opt?)
              (begin
                (for-each (H warn "unused label:") unused)
                (resolve env* code*))))))

    (define *prelude*
      (file->sexps "nienor/prelude.scm"))

    (define (attach-prelude lst)
      (append *prelude* lst))

    (define (compile-file filename opt? . att)
      (compile ((if (null? att) attach-prelude (car att)) (file->sexps filename)) opt?))

    ))
