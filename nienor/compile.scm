(define-library (nienor compile)
  (import
   (owl toplevel)
   (nienor common)
   (nienor macro)
   (nienor optimize))

  (export
   expand-macros
   attach-prelude
   compile
   compile-file)

  (begin
    (define (gensym)
      (interact 'gensym '_))

    (define (start-gensym!)
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
              (loop (+ n 1))))))))

    (define (kill-gensym!)
      (interact 'gensym (tuple 'exit!)))

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

    (define (name->skip-prologue-name name)
      (string->symbol (str name "__skip-prologue")))

    (define (make-defun codegen cont env acc rest at name args body mode)
      (lets ((f (codegen
                   at
                   `((define-label! ,name)
                     (_with-locals!
                      ,(reverse args)
                      ((define-label! ,(name->skip-prologue-name name)) ,@body))
                     ,@(if (eq? mode 'vector)
                           '((uxn-call! () brk)) ; a list because i had something something in mind
                           '((uxn-call! (2 r) jmp)))
                     )))
             (at code env* (f env)))
        (cont rest at env* (append acc (list (tuple 'commentary `(defun ,mode ,name ,args))) code))))

    (define (add-label env name value)
      (let ((labels (get env 'labels empty)))
        (put env 'labels (put labels name value))))

    ;; comment is a list
    (define (with-comment comment tpl)
      (list
       (tuple 'commentary comment)
       tpl))

    (define (which-local env local)
      (let loop ((l (get env 'locals #n)) (n 0))
        (cond
         ((null? l) #f)
         ((eq? local (car l)) n)
         (else
          (loop (cdr l) (+ n 1))))))

    (define (codegen at lst)
      (λ (env) ; env -> (values at code env')
        (let loop ((lst lst)
                   (at at)
                   (env env)
                   (acc #n))
          (if (null? lst)
              (values at acc env)
              (lets ((exp rest lst))
                (if (list? exp)
                    (tuple-case (list->tuple exp)
                      ((define-label! label)
                       (loop rest
                             at
                             (add-label env label at)
                             (append acc (list (tuple 'commentary `("label:" ,label))))))
                      ((define-constant name value)
                       (loop rest
                             at
                             (add-label env name value)
                             acc))
                      ((nalloc! name n-bytes)
                       (loop rest
                             (+ at n-bytes)
                             (add-label env name at)
                             (append acc (with-comment `("nalloc!" ,name ,n-bytes) (tuple 'bytes (make-list n-bytes 0))))))
                      ((_alloc! name exps)
                       (let* ((tuples
                              (map
                               (λ (exp)
                                 (cond
                                  ((string? exp)
                                   (tuple 'bytes (string->bytes exp)))
                                  ((symbol? exp)
                                   (tuple 'unresolved-symbol exp (λ (loc) (list (>> (band #xff00 loc) 8) (band #xff loc)))))
                                  ((number? exp)
                                   (if (> exp 255)
                                       (let ((l (list (>> (band #xff00 exp) 8) (band #xff exp))))
                                         (warn
                                          "_alloc:" exp
                                          "is > 255, allocating it as 2 bytes. consider doing this explicitly with these values: "
                                          l "to silence this warning")
                                         (tuple 'bytes l))
                                       (tuple 'bytes (list exp))))
                                  (else
                                   (error "i don't know what type" exp "is"))))
                               exps))
                              (len (fold
                                    (λ (a b)
                                      (tuple-case b
                                        ((bytes l)
                                         (+ a (len l)))
                                        ((unresolved-symbol _1 _2)
                                         (+ a 2))
                                        (else
                                         (error "BUG" b))))
                                    0 tuples)))
                         (loop rest
                               (+ at len)
                               (add-label env name at)
                               (append
                                acc
                                (list (tuple 'commentary `("space allocated for" ,name "(" ,len "bytes )")))
                                tuples))))
                      ((codegen-at! ptr)
                       (loop rest ptr env (append acc (list (tuple 'codegen-at ptr)))))
                      ((_push! mode value)
                       (lets ((byte? (eq? mode 'byte))
                              (env at code
                                   (cond
                                    ((number? value)
                                     (if byte?
                                         (values env (+ at 2) (list (tuple 'bytes `(,LIT ,(band #xff value)))))
                                         (values env (+ at 3) (list (tuple 'bytes `(,(short! LIT) ,(>> (band #xff00 value) 8) ,(band #xff value)))))))
                                    ((eq? value #t)
                                     (if byte?
                                         (values env (+ at 2) (list (tuple 'bytes `(,LIT 1))))
                                         (values env (+ at 3) (list (tuple 'bytes `(,(short! LIT) 0 1))))))
                                    ((eq? value #f)
                                     (if byte?
                                         (values env (+ at 2) (list (tuple 'bytes `(,LIT 0))))
                                         (values env (+ at 3) (list (tuple 'bytes `(,(short! LIT) 0 0))))))
                                    ((symbol? value)
                                     (let loop ((l (get env 'locals #n)) (n 0))
                                       (cond
                                        ((null? l)
                                         ;; did not found value in locals, try searching in function labels
                                         (lets ((variable? (has? (get env 'vars #n) value))
                                                (resolve size (cond
                                                               ((and byte? variable? (not (eq? mode 'no-get-var!)))
                                                                (values
                                                                 (λ (loc)
                                                                   `(,(short! LIT) ,(>> (band #xff00 loc) 8) ,(band #xff loc) ,LDA))
                                                                 4))
                                                               ((and variable? (not (eq? mode 'no-get-var!)))
                                                                (values
                                                                 (λ (loc)
                                                                   `(,(short! LIT) ,(>> (band #xff00 loc) 8) ,(band #xff loc) ,(short! LDA)))
                                                                 4))
                                                               (byte?
                                                                (values
                                                                 (λ (loc)
                                                                   `(,LIT ,(band #xff loc)))
                                                                 2))
                                                               (else
                                                                (values
                                                                 (λ (loc)
                                                                   `(,(short! LIT) ,(>> (band #xff00 loc) 8) ,(band #xff loc)))
                                                                 3)))))
                                           (values
                                            env
                                            (+ at size)
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
                                     (lets ((f (codegen at (list value)))
                                            (at code env (f env)))
                                       (values env at code)))
                                    ((string? value)
                                     (let ((raw (fold append #n (map (λ (x) `(,(short! LIT) 0 ,x)) (reverse (string->bytes value))))))
                                       (values env (+ at (len raw)) (list (tuple 'bytes raw)))))
                                    (else
                                     (error "unsupported type for _push!: " value)))))
                         (loop rest at env (append acc
                                                   (if (list? value) '() (list (tuple 'commentary `(_push! ,value)))) ; don't comment resolving lists
                                                   code))))
                      ((_with-locals! names body)
                       (lets ((resolve (λ (loc) ; loc = ___alloc-arg
                                         `(,(short! LIT) ,(>> (band #xff00 loc) 8) ,(band #xff loc)
                                           ,@(map (λ (_) (keep! (short! JSR))) names)
                                           ,(short! POP))))
                              (n (len names))
                              (code*-len (+ 4 n))
                              (env (put env 'locals (append names (get env 'locals #n))))
                              (free (if (= n 0)
                                        (tuple 'commentary '(removed freeing of 0 locals))
                                        (tuple 'bytes
                                               `(,LIT 0 ,LDZ  ; load local ptr from 0x0
                                                 ,LIT ,n ,SUB ; subtract the amount of freed locals
                                                 ,LIT 0 ,STZ  ; save new local ptr to 0x0
                                                 ))))
                              (f (codegen (+ at code*-len) body))
                              (at code env (f env))
                              (env*
                               (put env 'locals (let loop ((lst (get env 'locals #n)) (n n))
                                                  (if (= n 0)
                                                      lst
                                                      (loop (cdr lst) (- n 1)))))))
                         (loop rest
                               (+ at (if (= n 0) 0 (len (ref free 2))))
                               env*
                               (append
                                acc
                                (list (tuple 'commentary `(_with-locals! ,names ";; locals =" ,(get env 'locals #n))))
                                (list (tuple 'unresolved-symbol '___alloc-arg resolve))
                                code
                                (list (tuple 'commentary `(freeing ,n locals ";; locals =" ,(get env* 'locals #n))))
                                (list free)))))
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
                       (if (and (eq? func 'nigeb) (get env 'opt? #f)) ; TODO: generalize
                           (loop rest at env (append acc (list (tuple 'commentary '(removed nigeb call)))))
                           (lets ((f (codegen at `((_push! _ ,func))))
                                  (at code env* (f env)))
                             (loop
                              rest
                              (+ at 1) ; +1 because of JSR2
                              env*
                              (append
                               acc                                          ; old code
                               (list (tuple 'commentary `(funcall! ,func))) ; comment
                               code                                         ; push function
                               (list (tuple 'bytes `(,(short! JSR))))       ; jump
                               )))))
                      ((if arg then else)
                       (lets ((func (car* exp))
                              (args (cdr* exp))
                              (end (gensym))
                              (then-loc (gensym))
                              (f (codegen at `(,arg
                                               (uxn-call! () ora) ; <- arg is a short - we OR it to get any truthy values into one byte
                                               (_push! _ ,then-loc)
                                               (uxn-call! (2) jcn)
                                               ,else
                                               (_push! _ ,end)
                                               (uxn-call! (2) jmp)
                                               (define-label! ,then-loc)
                                               ,then
                                               (define-label! ,end))))
                              (at code env* (f env)))
                         (loop rest at env* (append acc (list (tuple 'commentary `(if ,arg))) code))))
                      ((_with-label label body)
                       (lets ((label-was (get (get env 'labels) label #f))
                              (env (add-label env label at))
                              (f (codegen at body))
                              (at code env* (f env)))
                         (loop rest
                               at
                               (if label-was
                                   (put env* 'labels (put (get env* 'labels empty) label label-was))
                                   (put env* 'labels (del (get env* 'labels empty) label)))
                               (append acc code))))
                      ((_λ args body)
                       (let ((used-locals (intersect (get env 'locals #n) (code->used-symbols body))))
                         (if (null? used-locals)
                             (let ((name (gensym)) ; a normal lambda
                                   (resolve (λ (loc) `(,(short! LIT) ,(>> (band #xff00 loc) 8) ,(band #xff loc)))))
                               (loop
                                rest
                                (+ at 3)
                                (put env 'epilogue (append (get env 'epilogue #n) `((_defun ,name ,args ,body))))
                                (append acc (with-comment `(λ ,name ,args) (tuple 'unresolved-symbol name resolve)))))
                             (lets ((name (gensym)) ; FUCK! it's a closure
                                    (func `((_defun ,name (,@used-locals ,@args) ,body)))
                                    (clos-size (+ (* (len used-locals) 3) 4))
                                    (resolve1 (λ (loc)                                                     ; loc = malloc
                                                `(,(short! LIT) ,(>> (band #xff00 clos-size) 8) ,(band #xff clos-size) ; push closure size
                                                  ,(short! LIT) ,(>> (band #xff00 loc) 8) ,(band #xff loc) ; push malloc
                                                  ,(short! JSR)                                            ; malloc(n)
                                                  ,(short! DUP)                                            ; ptr* ptr*
                                                  ,@(fold
                                                     append
                                                     #n
                                                     (map (λ (local)
                                                            (let ((n (which-local env local)))
                                                              `(,LIT ,(short! LIT)                                 ; push LIT2
                                                                ,ROT ,ROT                                          ; ptr* LIT -- LIT ptr*
                                                                ,(keep! STA)                                       ; save LIT
                                                                ,ROT ,POP                                          ; drop LIT
                                                                ,(short! INC)                                      ; ptr* ++
                                                                ,LIT 0 ,LDZ ,LIT ,n ,SUB ,LIT 2 ,MUL ,(short! LDZ) ; load local onto stack
                                                                ,(short! SWP)                                      ; ptr* local* -- local* ptr*
                                                                ,(short! (keep! STA))                              ; save local
                                                                ,(short! NIP)                                      ; local* ptr* -- ptr*
                                                                ,(short! INC)
                                                                ,(short! INC)                                      ; ptr* += 2
                                                                )))
                                                          (reverse used-locals)))
                                                  ,LIT ,(short! LIT)
                                                  ,ROT ,ROT
                                                  ,(keep! STA)))) ; save LIT
                                    (resolve2 (λ (loc)                                                     ; loc = the gensymmed lambda
                                                `(,ROT ,POP                                                ; drop LIT
                                                  ,(short! INC)                                            ; ptr* ++
                                                  ,(short! LIT) ,(>> (band #xff00 loc) 8) ,(band #xff loc) ; push the lambda
                                                  ,(short! SWP)
                                                  ,(short! (keep! STA))                                    ; save lambda
                                                  ,(short! NIP)                                            ; λ ptr* -- ptr*
                                                  ,(short! INC)
                                                  ,(short! INC)                                            ; ptr* += 2
                                                  ,LIT ,(short! JMP)
                                                  ,ROT ,ROT
                                                  ,STA
                                                  ;; we're left with the old ptr to the generated "function"
                                                  )))
                                    (length (+ 29 (* (len used-locals) 23))))
                               (loop
                                rest
                                (+ at length)
                                (put env 'epilogue (append (get env 'epilogue #n) func))
                                (append acc (list (tuple 'commentary `(λ CLOSURE ,name "[" ,used-locals "]" ,args))
                                                  (tuple 'unresolved-symbol 'malloc resolve1)
                                                  (tuple 'unresolved-symbol name resolve2))))
                             ))))
                      ((_tailcall! name args n-locals)
                       (lets ((f (codegen at `(,@(map (λ (a) `(_push! _ ,a)) args))))
                              (at code env* (f env))
                              (resolve (λ (loc) `(;; 1. free all locals that are not function arguments
                                                  ;; TODO: DRY with _with-locals!
                                                  ,LIT 0 ,LDZ         ; load local ptr from 0x0
                                                  ,LIT ,n-locals ,SUB ; subtract the amount of freed locals
                                                  ,LIT 0 ,STZ         ; save new local ptr to 0x0

                                                  ;; 2. do the tailcall
                                                  ,LIT 0 ,LDZ ,LIT 2 ,MUL ; load ptr and *2 it because every local is a short
                                                  ,@(fold
                                                     append #n
                                                     (map (λ (_)
                                                            `(,(keep! (short! STZ)) ; store arg in zero-page @ ptr
                                                              ,NIP ,NIP             ; remove arg, keep ptr
                                                              ,LIT 2 ,SUB           ; ptr -= 2, jump to another arg
                                                              ))
                                                          args))
                                                  ,POP ; pop ptr
                                                  ,(short! LIT) ,(>> (band #xff00 loc) 8) ,(band #xff loc) ; push function
                                                  ,(short! JMP) ; jump without a real funcall
                                                  )))
                              (length (+ 20 (* (len args) 6))))
                         (loop rest (+ at length) env*
                               (append acc code (with-comment
                                                 `(tailcall! ,name)
                                                 (tuple 'unresolved-symbol (name->skip-prologue-name name) resolve))))))
                      ((_get! addr)
                       (lets ((f (codegen at `((_push! _ ,addr) (uxn-call! (2) lda))))
                              (at code env (f env)))
                         (loop rest at env (append acc code))))
                      ((_addrof thing)
                       (lets ((f (codegen at (if (has? (get env 'vars #n) thing)
                                                 `((_push! no-get-var! ,thing))
                                                 `(,thing))))
                              (at code env (f env)))
                         (loop rest at env (append acc code))))
                      (else ; funcall OR ignored
                       (lets ((func (car* exp))
                              (args (cdr* exp))
                              (f (codegen at `(,@(map (λ (a) `(_push! _ ,a)) (reverse args)) (funcall! ,func))))
                              (at code env* (f env)))
                         (loop rest at env* (append acc code)))))
                    (loop `((_push! _ ,exp) ,@rest) at env acc) ; if we got an atom, just push it
                    ))))))

    ;; env exp → env' exp'
    (define (lookup-toplevel-macros env exp)
      (let loop ((exp exp) (env env) (acc #n) (substitutions 0))
        (if (null? exp)
            (values substitutions env acc)
            (tuple-case (list->tuple (car* exp))
              ((define-macro-rule literal rule rewrite)
               (loop (cdr exp) (add-macro env rule rewrite literal) acc (+ substitutions 1)))
              ((_flatten! code)
               (loop (append code (cdr exp)) env acc (+ substitutions 1)))
              ((_include! filename)
               (loop (append (file->sexps filename) (cdr exp)) env acc (+ substitutions 1)))
              ((_declare-var! var-name)
               (loop (cdr exp) (put env 'vars (cons var-name (get env 'vars #n))) acc (+ substitutions 1)))
              (else
               (loop (cdr exp) env (append acc (list (car exp))) substitutions))))))

    (define (expand-macros lst env)
      (lets ((substitutions env* lst (lookup-toplevel-macros env lst)))
        (if (= substitutions 0)
            (values
             env*
             (apply-macros env* lst))
            (expand-macros (apply-macros env* lst) env*))))

    (define (resolve env lst add-debug-info?)
      (fold (λ (a b)
              (tuple-case b
                ((commentary exp)
                 (if add-debug-info?
                     (append a (list exp))
                     a))
                ((codegen-at ptr*)
                 (let ((length (if add-debug-info?
                                   (len (filter (B not list?) a))
                                   (len a))) ; ^- this is true for both modes, but just taking len is faster
                       (ptr (- ptr* #x100)))
                   (if (> (len a) ptr)
                       (error "attempting to codegen-at! on code that was already written " ptr)
                       (append a (make-list (- ptr length) 0)))))
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

    ;; TODO: Add some sort of env to hold all these options
    ;; with-debug? will ask (resolve) to attach comments about code into the resolving byte stream
    (define (compile lst opt? with-debug? only-expand-macros verbose?)
      (start-gensym!) ; a toplevel thread didn't seem to compile correctly

      (let loop ((lst lst) (at #x100) (code #n) (env (put empty-env 'opt? opt?)) (full-lst #n))
        (lets ((env lst (expand-macros lst env))
               (lst (if opt? (optimize lst verbose?) lst))
               (at code* env ((codegen at lst) env))
               (epilogue (get env 'epilogue #n))
               (env (put env 'epilogue #n))
               (full-lst (append full-lst lst)))
          (if (null? epilogue)
              (begin
                (kill-gensym!)
                (if only-expand-macros
                    (only-expand-macros full-lst)
                    (values env (resolve
                                 (put env 'labels (put (get env 'labels empty) '*compiler-end* at))
                                 (append code code*) with-debug?))))
              (loop epilogue at (append code code*) env full-lst)))))

    (define *prelude*
      (file->sexps "nienor/lib/prelude.scm"))

    (define (attach-prelude lst)
      (append *prelude* lst))

    (define (compile-file filename opt? verbose?)
      (compile (attach-prelude (file->sexps filename)) opt? #f #f verbose?))
    ))
