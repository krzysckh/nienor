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
        (cont rest at env* (append acc (list (tuple 'commentary `(defun ,mode ,name ,args))) code))))

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

    ;; mark label (defun) as important (don't delete it when of no use)
    (define (mark-important env name)
      (put env 'unused (put (get env 'unused empty) name #f)))

    (define (remove-unused env name)
      (put env 'unused (put (get env 'unused empty) name #f)))

    ;; comment is a list
    (define (with-comment comment tpl)
      (list
       (tuple 'commentary comment)
       tpl))

    (define (codegen at lst)
      `(,at
        . ,(λ (env) ; env -> (values at code env')
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
                                  (append acc (list (tuple 'commentary `("label:" ,label))))))
                           ((define-constant name value)
                            (loop rest
                                  at
                                  (add-label env name value 'nah)
                                  acc))
                           ((nalloc! name n-bytes)
                            (loop rest
                                  (+ at n-bytes)
                                  (add-label env name at)
                                  (append acc (with-comment `("nalloc!" ,name ,n-bytes) (tuple 'bytes (make-list n-bytes 0))))))
                           ((_alloc! name bytes)
                            (let ((bytes (fold
                                          (λ (a b)
                                            (append
                                             a
                                             (cond
                                              ((string? b) (string->bytes b))
                                              ((symbol? b) (error "_alloc! with variables is not implemented"))
                                              ((number? b)
                                               (if (> b 255)
                                                   (let ((l (list (>> (band #xff00 b) 8) (band #xff b))))
                                                     (warn
                                                      "_alloc:" b
                                                      "is > 255, allocating it as 2 bytes. consider doing this explicitly with these values: "
                                                      l "to silence this warning")
                                                     l)
                                                   (list b)))
                                              (else
                                               (error "i don't know what type" b "is")))))
                                          #n
                                          bytes)))
                              (loop rest
                                    (+ at (len bytes))
                                    (add-label env name at)
                                    (append acc (with-comment `("_alloc!" ,name ,(len bytes)) (tuple 'bytes bytes))))))
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
                              (loop rest at env (append acc
                                                        (if (list? value) '() (list (tuple 'commentary `(_push! ,value)))) ; don't comment resolving lists
                                                        code))))
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
                                    (append acc (with-comment `(free-locals! ,n) (tuple 'bytes code))))))
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
                               (append acc (with-comment `(allocate-local! ,name) (tuple 'bytes code))))))
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
                                (lets ((dat (codegen at `((_push! _ ,func))))
                                       (_ f dat)
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
                              (loop rest at env* (append acc (list (tuple 'commentary `(if ,arg))) code))))
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
                           ((_λ args body)
                            (let ((name (gensym))
                                  (resolve (λ (loc) `(,LIT ,(>> (band #xff00 loc) 8) ,LIT ,(band #xff loc)))))
                              (loop
                               rest
                               (+ at 4)
                               (mark-important
                                (put env 'epilogue (append (get env 'epilogue #n) `((_defun ,name ,args ,body))))
                                name)
                               (append acc (with-comment `(λ ,name ,args) (tuple 'unresolved-symbol name resolve))))))
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
        (put 'labels   empty) ; ff of labels & constants, global
        (put 'locals   #n)    ; a list, newest consed before, then removed at free-locals!
        (put 'macros   empty) ; ff of macro-name -> λ (exp) -> rewritten
        (put 'unused   empty) ; ff of label -> truthy value when unused
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

    (define unused-no-complain '(nigeb))

    (define (get-unused env)
      (filter
       (λ (e) (not (has? unused-no-complain e)))
       (ff-fold
        (λ (a k v) (if v (cons k a) a))
        #n
        (get env 'unused empty))))

    (define (delete-unused-defuns unused lst verbose?)
      (let loop ((lst lst) (acc #n))
        (cond
         ((null? lst) acc)
         ((and (eq? (car* (car lst)) '_defun)
               (has? unused (cadr (car lst))))
          (when verbose?
            (print "  Deleted " (cadr (car lst))))
          (loop (cdr lst) acc))
         (else
          (loop (cdr lst) (append acc (list (car lst))))))))

    (define (codegen-until-empty-epilogue at lst env)
      (lets ((at code* env* ((cdr (codegen at lst)) env))
             (epilogue (get env* 'epilogue #n)))
        (if (null? epilogue)
            (values at code* env*)
            (lets ((at code env (codegen-until-empty-epilogue at epilogue (put env* 'epilogue #n))))
              (values
               at
               (append code* code)
               env)))))

    (define (uniq lst)
      (let loop ((lst lst) (acc #n))
        (cond
         ((null? lst) acc)
         ((has? acc (car lst)) (loop (cdr lst) acc))
         (else
          (loop (cdr lst) (cons (car lst) acc))))))

    (define (defun->used-symbols exp)
      (uniq
       (filter
        symbol?
        (let walk ((exp (cdddr exp)) (acc #n))
          (cond
           ((null? exp) acc)
           ((pair? (car exp)) (walk (car exp) (append acc (walk (cdr exp) #n))))
           (else
            (walk (cdr exp) (append (list (car exp)) acc))))))))

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

    ;; This is a better way of finding unused functions.
    ;; the faster thing is done during codegen, labels that don't have users get marked, and then defuns
    ;; with them get removed. This function looks from main and tries to find all functions that _are_
    ;; used, then when we subtract this from "all functions list" we get the list of unused functions :p
    (define (find-unused lst env)
      (let ((used
             (let ((main (find-defun lst 'main)))
               (let loop ((seen '(main)) (syms (defun->used-symbols main)))
                 (if (null? syms)
                     seen
                     (if-lets ((fun (find-defun lst (car syms))))
                       (loop (cons (car syms) seen)
                             (append (cdr syms) (filter
                                                 (λ (s) (not (has? seen s)))
                                                 (defun->used-symbols fun))))
                       (loop seen (cdr syms))))))))
        (keys (fold del (get env 'labels empty) used))))

    ;; TODO: Add some sort of env to hold these options
    ;; with-debug? will ask (resolve) to attach comments about code into the resolving byte stream
    (define (compile lst opt? with-debug? only-expand-macros verbose?)
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
                    ;; ((eq? opt? 0) #f)
                    ((eq? opt? #f) #f)
                    ((not (number? opt?)) 4)
                    (else opt?)))
             (env (put empty-env 'opt? opt?))
             (env lst (expand-macros lst env))
             (_ code* env* (codegen-until-empty-epilogue #x100 lst env))) ; <- gensym is only needed here
        (interact 'gensym (tuple 'exit!))                                 ; so we can kill it afterwards
        (let ((unused (get-unused env*)))
          (when verbose?
            (print "Cleanup level: " opt?))
          (cond
           ((and (number? opt?) (> opt? 0) (not (null? unused)))
            (compile
             (delete-unused-defuns unused lst verbose?) ; delete unneeded function definitions
             (- opt? 1) with-debug? only-expand-macros verbose?))
            ((or (and (number? opt?) (= opt? 1)) (and (> opt? 0) (null? unused)))
             ;; aggresively delete unused once
             ;; TODO: check if only doing this wouldn't be faster than literally re-compiling
             ;;       everything 3 or so times LMAO
             ;; TODO: yeah disable the on-the-run label marking and just use this
             (let ((l (find-unused lst env*)))
               (compile
                (delete-unused-defuns l lst verbose?) ; delete unneeded function definitions
                (- opt? 1) with-debug? only-expand-macros verbose?)))
            (else
             (if only-expand-macros
                 (only-expand-macros lst)
                 (let ()
                   (for-each (H warn "unused label:") unused)
                   (values env* (resolve env* code* with-debug?)))))))))

    (define *prelude*
      (file->sexps "nienor/prelude.scm"))

    (define (attach-prelude lst)
      (append *prelude* lst))

    (define (compile-file filename opt? att verbose?)
      (compile ((if att att attach-prelude) (file->sexps filename)) opt? #f #f verbose?))

    ))
