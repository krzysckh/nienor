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
                     ,@(map (λ (s) `(allocate-local! ,s)) args) ; bump local counter in zero page, arg to zero-page
                     (define-label! ,(name->skip-prologue-name name)) ; TODO: only generate this in functions with tailcalls
                     ,@body
                     (free-locals! ,(len args))
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

    (define (uniq lst)
      (let loop ((lst lst) (acc #n))
        (cond
         ((null? lst) acc)
         ((has? acc (car lst)) (loop (cdr lst) acc))
         (else
          (loop (cdr lst) (cons (car lst) acc))))))

    (define *symbols-used-internally* '(nigeb *rt-finish*))

    (define (code->used-symbols exp)
      (uniq
       (append
        *symbols-used-internally*
        (filter
         symbol?
         (let walk ((exp exp) (acc #n))
           (cond
            ((null? exp) acc)
            ((pair? (car exp)) (walk (car exp) (append acc (walk (cdr exp) #n))))
            (else
             (walk (cdr exp) (append (list (car exp)) acc)))))))))

    (define (defun->used-symbols exp)
      (code->used-symbols (cdddr exp)))

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
                                         (let ((resolve (λ (loc)
                                                          (if byte?
                                                              `(,LIT ,(band #xff loc))
                                                              `(,(short! LIT) ,(>> (band #xff00 loc) 8) ,(band #xff loc))))))
                                           ;; TODO: remove magic length, dry with funcall!
                                           (values
                                            env
                                            (+ at (if byte? 2 3))
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
                      ((free-locals! n)
                       (if (eq? n 0)
                           (loop rest at env (append acc (list (tuple 'commentary '(removed free-locals! 0))))) ; this is a peephole optimization
                           (let ((code `(,LIT 0 ,LDZ  ; load local ptr from 0x0
                                         ,LIT ,n ,SUB ; subtract the amount of freed locals
                                         ,LIT 0 ,STZ  ; save new local ptr to 0x0
                                         ))
                                 (env
                                   (put env 'locals (let loop ((lst (get env 'locals #n)) (n n))
                                                        (if (= n 0)
                                                            lst
                                                            (loop (cdr lst) (- n 1)))))))
                             (loop rest
                                   (+ at (len code))
                                   env
                                   (append acc (with-comment `(free-locals! ,n ";; locals =" ,(get env 'locals #n)) (tuple 'bytes code)))))))
                      ((allocate-local! name)
                       (let ((code `(,LIT 0 ,LDZ      ; load ptr
                                     ,INC             ; inc ptr
                                     ,LIT 2 ,MUL      ; *2 because all locals are shorts
                                     ,(short! STZ)    ; store arg at ptr
                                     ,LIT 0 ,LDZ ,INC ; load & inc ptr again
                                     ,LIT 0 ,STZ      ; store new ptr at 0x0
                                     ))
                             (env (put env 'locals (cons name (get env 'locals #n)))))
                         (loop
                          rest
                          (+ at (len code))
                          env
                          (append acc (with-comment `(allocate-local! ,name ";; locals =" ,(get env 'locals #n))
                                                    (tuple 'bytes code))))))
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
                                    (resolve1 (λ (loc)                                                     ; loc = *rt-finish*
                                                `(,(short! LIT) ,(>> (band #xff00 loc) 8) ,(band #xff loc) ; push *rt-finish*
                                                  ,(short! (keep! LDA))                                    ; get! *rt-finish*
                                                  ,(short! DUP)                                            ; rtf* ptr* ptr*
                                                  ,(short! ROT)                                            ; ptr* ptr* rtf*
                                                  ,(short! SWP)                                            ; ptr* (<- this is saved as a final ptr) rtf* ptr*
                                                  ,@(fold
                                                     append
                                                     #n
                                                     (map (λ (local)
                                                            (let ((n (which-local env local)))
                                                              `(,LIT ,(short! LIT)                                 ; push LIT2
                                                                ,ROT ,ROT                                          ; ptr* LIT -- LIT ptr*
                                                                ,(keep! STA)                                       ; save LIT to *rt-finish*
                                                                ,ROT ,POP                                          ; drop LIT
                                                                ,(short! INC)                                      ; ptr* ++
                                                                ,LIT 0 ,LDZ ,LIT ,n ,SUB ,LIT 2 ,MUL ,(short! LDZ) ; load local onto stack
                                                                ,(short! SWP)                                      ; ptr* local* -- local* ptr*
                                                                ,(short! (keep! STA))                              ; save local to *rt-finish*
                                                                ,(short! NIP)                                      ; local* ptr* -- ptr*
                                                                ,(short! LIT) 0 2
                                                                ,(short! ADD)                                      ; ptr* += 2
                                                                ,(short! SWP)                                      ; *rt-finish* ptr* -- ptr* *rt-finish*
                                                                ,(keep! (short! STA))                              ; update *rt-finish*
                                                                ,(short! SWP)                                      ; ptr* *rt-finish* -- *rt-finish* ptr*
                                                                )))
                                                          (reverse used-locals)))
                                                  ,LIT ,(short! LIT)
                                                  ,ROT ,ROT
                                                  ,(keep! STA)))) ; save LIT to *rt-finish*
                                    (resolve2 (λ (loc)                                                     ; loc = the gensymmed lambda
                                                `(,ROT ,POP                                                ; drop LIT
                                                  ,(short! INC)                                            ; ptr* ++
                                                  ,(short! LIT) ,(>> (band #xff00 loc) 8) ,(band #xff loc) ; push the lambda
                                                  ,(short! SWP)
                                                  ,(short! (keep! STA))                                    ; save lambda to *rt-finish*
                                                  ,(short! NIP)                                            ; λ ptr* -- ptr*
                                                  ,(short! LIT) 0 2
                                                  ,(short! ADD)                                            ; ptr* += 2
                                                  ,LIT ,(short! JMP)
                                                  ,ROT ,ROT
                                                  ,(keep! STA)
                                                  ,ROT ,POP
                                                  ,(short! INC)
                                                  ,(short! SWP)                                            ; *rt-finish* ptr* -- ptr* *rt-finish*
                                                  ,(short! STA)                                            ; update *rt-finish*
                                                  ;; we're left with the old ptr to the generated "function"
                                                  )))
                                    (length (+ 35 (* (len used-locals) 28))))
                               (loop
                                rest
                                (+ at length)
                                (put env 'epilogue (append (get env 'epilogue #n) func))
                                (append acc (list (tuple 'commentary `(λ CLOSURE ,name "[" ,used-locals "]" ,args))
                                                  (tuple 'unresolved-symbol '*rt-finish* resolve1)
                                                  (tuple 'unresolved-symbol name resolve2))))
                             ))))
                      ((_tailcall! name args)
                       (lets ((f (codegen at `(,@(map (λ (a) `(_push! _ ,a)) args))))
                              (at code env* (f env))
                              (resolve (λ (loc) `(,LIT 0 ,LDZ ,LIT 2 ,MUL ; load ptr and *2 it because every local is a short
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
                              (length (+ 11 (* (len args) 6))))
                         (loop rest (+ at length) env*
                               (append acc code (with-comment
                                                 `(tailcall! ,name)
                                                 (tuple 'unresolved-symbol (name->skip-prologue-name name) resolve))))))
                      (else ; funcall OR ignored
                       (lets ((func (car* exp))
                              (args (cdr* exp))
                              (f (codegen at `(,@(map (λ (a) `(_push! _ ,a)) (reverse args)) (funcall! ,func))))
                              (at code env* (f env)))
                         (loop rest at env* (append acc code)))))
                    (loop `((_push! _ ,exp) ,@rest) at env acc) ; if we got an atom, just push it
                    ))))))

    (define empty-env
      (pipe empty
        (put 'labels   empty) ; ff of labels & constants, global
        (put 'locals   #n)    ; a list, newest consed before, then removed at free-locals!
        (put 'macros   empty) ; ff of macro-name -> λ (exp) -> rewritten
        (put 'opt?     #f)    ; should code be optimised?
        (put 'epilogue #n)    ; epilogue compiled after 1st codegen pass.
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
           ((atom? exp) exp)
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
              ((_flatten! code)
               (loop (append code (cdr exp)) env acc (+ substitutions 1)))
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

    (define (find-defun lst name)
      (let loop ((lst lst))
        (cond
         ((null? lst) #f)
         ((and (or (eq? (car* (car lst)) '_defun)
                   (eq? (car* (car lst)) '_defun-vector))
               (eq? (cadr (car lst)) name))
          (car lst))
         (else
          (loop (cdr lst))))))

    ;; This function looks from main and tries to find all functions that _are_ used
    (define (find-used lst)
      (if-lets ((main (find-defun lst 'main)))
        (let loop ((seen '(main)) (syms (defun->used-symbols main)))
          (if (null? syms)
              seen
              (if-lets ((fun (find-defun lst (car syms))))
                (loop (cons (car syms) seen)
                      (append (cdr syms) (filter
                                          (λ (s) (not (has? seen s)))
                                          (defun->used-symbols fun))))
                (loop (cons (car syms) seen) (cdr syms)))))
        #f))

    ;; this removes unused defuns, defun-vectors, _alloc!s and nalloc!s
    (define (keep-only-used-defuns lst verbose?)
      (if-lets ((used (find-used lst)))
        (let loop ((lst lst) (acc #n))
          (cond
           ((null? lst) acc)
           ((and (or (eq? (car* (car lst)) '_defun)
                     (eq? (car* (car lst)) '_defun-vector)
                     (eq? (car* (car lst)) '_alloc!)
                     (eq? (car* (car lst)) 'nalloc!))
                 (not (has? used (cadr (car lst)))))
            (when verbose?
              (print "  Deleted " (cadr (car lst))))
            (loop (cdr lst) acc))
           (else
            (loop (cdr lst) (append acc (list (car lst)))))))
        lst)) ; failed to find main, don't apply anything (it's probably an epilogue pass)

    (define (but-last lst)
      (cond
       ((null? lst) #n)
       ((null? (cdr* lst)) #n)
       (else
        (cons (car* lst) (but-last (cdr* lst))))))

    (define (maybe-tailcall defun)
      (let ((name (cadr defun))
            (code (cadddr defun)))
        `(_defun
          ,name ,(caddr defun)
          ,(append
            (but-last code)
            (list
             (let loop ((e (last code #f)))
               (cond
                ((eq? (car* e) 'nigeb)
                 (if (null? (cdr* e))
                     '(nigeb)
                     `(nigeb ,(loop (car* (cdr* e))) ,@(cdr* (cdr* e)))))
                ((eq? (car* e) 'if)
                 `(if ,(cadr e) ,(loop (caddr e)) ,(loop (cadddr e))))
                (else
                 (if (eq? (car* e) name)
                     `(_tailcall! ,name ,(cdr e))
                     e)))))))))

    (define (find-tailcalls lst)
      (let loop ((lst lst) (acc #n))
        (let ((exp (car* lst)))
          (cond
           ((null? lst) acc)
           ((eq? (car* exp) '_defun)
            (let ((mt (maybe-tailcall exp)))
              (loop (cdr lst) (append acc (list mt)))))
           (else
            (loop (cdr lst) (append acc (list exp))))))))

    (define (optimize lst verbose?)
      (lets ((lst (keep-only-used-defuns lst verbose?)) ; delete unused defuns that would eat up space
             (lst (find-tailcalls lst)))                ; add TCO marks
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
               (env (put env 'epilogue #n)))
          (if (null? epilogue)
              (begin
                (kill-gensym!)
                (if only-expand-macros
                    (only-expand-macros full-lst)
                    (values env (resolve
                                 (put env 'labels (put (get env 'labels empty) '*compiler-end* at))
                                 (append code code*) with-debug?))))
              (loop epilogue at (append code code*) env (append full-lst lst))))))

    (define *prelude*
      (file->sexps "nienor/prelude.scm"))

    (define (attach-prelude lst)
      (append *prelude* lst))

    (define (compile-file filename opt? att verbose?)
      (compile ((if att att attach-prelude) (file->sexps filename)) opt? #f #f verbose?))
    ))
