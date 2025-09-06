(define-library (nienor typecheck)
  (import
   (owl toplevel)
   (nienor common)
   (nienor optimize)
   (nienor macro))

  (export
   render-func
   types-of
   typecheck)

  (begin
    ;; ugly hack for recursion
    (define *max-typecheck-depth* 8)

    (define environment? function?)

    (define *effect-table*
      (list->ff ; TODO: check if true
       ;; nam takes returns
       '((brk 0 0)
         (inc 1 1)
         (pop 1 0)
         (nip 2 1)
         (swp 2 2)
         (rot 3 3)
         (dup 1 2)
         (ovr 2 3)
         (equ 2 1)
         (neq 2 1)
         (gth 2 1)
         (lth 2 1)
         (jmp 1 0)
         ;; (jcn 0 0)
         ;; (jsr 0 0)
         ;; (sth 0 0)
         ;; (ldz 1 1)
         ;; (stz 0 0)
         ;; (ldr 0 0)
         ;; (str 0 0)
         (lda 1 1)
         (sta 2 0)
         ;; (dei 0 0)
         ;; (deo 0 0)
         (add 2 1)
         (sub 2 1)
         (mul 2 1)
         (div 2 1)
         (and 2 1)
         (ora 2 1)
         (eor 2 1)
         (sft 2 1))))
         ;; (jci 0 0)
         ;; (jmi 0 0)
         ;; (jsi 0 0)
         ;; (lit 0 0))))

    ;; name env → argT resT
    (define (types-of f env)
      (if f
          (if-lets ((ff (get env 'tcheck #f))
                    (v  (get ff f #f)))
            (values (get v 'args #n) (get v 'result #f))
            (values #f #f))
            ;; (if-lets ((a (get (get env 'arity empty) f #f)))
            ;;   (values (make-list a 'Any) 'Any env)
            ;;   (values #f #f env)))
          (values #f #f)))

    (define imm-type
      (pipe empty
        (put 'Number  number?)
        (put 'String  string?)
        (put 'Symbol  symbol?)
        (put 'Bool    boolean?)
        (put 'Pointer self)
        (put 'Any     self)))

    (define (func->type func env)
      (lets ((argT resT (types-of func env)))
        (or resT 'Any)))

    (define (funcall->type funcall env)
      (func->type (car* funcall) env))

    (define (imm->type v env types)
      (cond
       ((boolean? v)           `(Bool . ,v))
       ((number? v)            `(Number . ,v))
       ((string? v)            `(String . ,v))
       ((symbol? v)            (if-lets ((r (get types v #f)))
                                 r
                                 (if (eq? 'Any (func->type v env))
                                     `(Any . ,v)
                                     `(Pointer . ,v)))) ; a function pointer OR a value TODO: variables, TODO: actual symbols
       ((list? v)              `(,(funcall->type v env) . ,v))
       (else
        #f)))

    (define special '(_begin uxn-call! _push!))

    (define (types->declaration l)
      (if (= (len l) 1)
          (car l)
          (fold
           (λ (a b) (str a " -> " b))
           (car* l)
           (cdr* l))))

    (define (boolize x)
      (and (not (eq? 0 x)) x))

    ;; is t1 of the broader type t2
    ;; t1 = (T . val*), t2 = T'
    (define (is-of-type? t1 t2 env)
      (or (eq? (car* t1) 'Any)
          (eq? (car* t1) (car* t2))
          (if-lets ((f (get (get (get env 'trules empty) (car* t1) empty) t2 #f)))
            (if (eq? (cdr* t1) '__from-type)
                (and f #t)
                (boolize (f (cdr* t1) env)))
            #f)))

    (define (render-func f env)
      (lets ((argT resT (types-of f env)))
        (format #f "~a :: ~a" f (types->declaration (append argT (list resT))))))

    (define (typecheck-funcall funcall fun-name arg-types* env error*)
      ;; (print "typecheck-funcall " funcall ": "  fun-name " " arg-types*)
      (if-lets ((argT resT (types-of fun-name env))
                (arg-types (map car arg-types*)))
        (if (has? special fun-name)
            (if (verbose? env)
                (begin (format stdout "    [typecheck] cannot typecheck `~a' — special funcall" funcall) #t)
                #t)
            (let loop ((required argT) (got arg-types*) (arg (cdr* funcall)))
              (cond
               ((and (null? got) (null? required))
                (if (eq? 'Void resT) #f resT))
               ((or (null? got) (null? required))
                (error* `(,(format #f "Invalid arity in function call `~a' — expected ~a, got ~a" funcall (map car argT) arg-types)
                          "Function declared as"
                          ,(format #f "  ~a :: ~a" fun-name (types->declaration (append (map car argT) (list resT))))
                          "Used as"
                          ,(format #f "  ~a :: ~a" fun-name (types->declaration (append arg-types (list resT)))))))
               (else
                (if (or (eq? (caar required) 'Any)
                        (is-of-type? (car got) (caar required) env))
                    (loop (cdr required) (cdr got) (cdr* arg))
                    (error* `(,(format #f "Mismatched types in function call `~a' — expected ~a, got ~a" funcall (map car argT) arg-types)
                              "Function declared as"
                              ,(format #f "  ~a :: ~a" fun-name (types->declaration (append (map car argT) (list resT))))
                              "Used as"
                              ,(format #f "  ~a :: ~a" fun-name (types->declaration (append arg-types (list resT))))
                              ,(format #f "~a~a cannot be treated as ~a" (caar got) (if arg (str " `" (car arg) "'") "") (caar required))))
                    )))))))

    (define (lwalk walk l stack types)
      (let loop ((stack stack) (types types) (l l))
        (if (null? l)
            (values stack types)
            (lets ((stack types (walk stack types (car l))))
              (loop stack types (cdr l))))))

    (define (walk-typecheck defun env skip name->defun error* depth)
      (when (> depth *max-typecheck-depth*)
        (skip "max typecheck depth exceeded"))
      (lets ((name (cadr   defun))
             (args (caddr  defun))
             (body (cadddr defun))
             (argT resT (types-of name env)))
        ;; TODO: report function name for easier searching
        (letrec ((walk
                  (λ (stack types code)
                    ;; (print "            stack: " stack)
                    ;; (print "[typecheck] f=" name " code: " code)
                    ;; (print "            types: " (ff->list types))

                    (cond
                     ((null? code) (values stack types))
                     ((imm? code)  (values (cons (imm->type code env types) stack) types))
                     ((and (list? code) (eq? (car code) '_typechecker-bogger-off!))
                      (skip "asked to bogger off"))
                     ((and (list? code) (eq? (car code) 'uxn-call!))
                      (if (equal? '(2) (cadr code))
                          (if-lets ((info (get *effect-table* (caddr code) #f))
                                    (takes (car info))
                                    (adds  (cadr info)))
                            (values
                             (append (make-list adds '(Any . |uxn-call expression|)) (drop stack takes))
                             types)
                            (skip "not found in effect table"))
                          (skip `("bad uxn-call in" ,code))))
                     ((and (list? code) (eq? (car code) '_λ))
                      (values (cons '(Pointer . 'λ) stack) types))
                     ((and (list? code) (eq? (car code) '_get!))
                      (values (cons '(Any . '_get!) stack) types))
                     ((and (list? code) (eq? (car code) '_symbol))
                      (values (cons `(Symbol . ,(cadr code)) stack) types))
                     ((and (list? code) (eq? (car code) '_push!))
                      (when (eq? (cadr code) 'byte)
                        (skip "byte _push!"))
                      (values (cons (imm->type (caddr code) env types) stack) types))
                     ((and (list? code) (eq? (car code) '_begin))
                      (lwalk walk (cadr code) stack types))
                     ((and (list? code) (eq? (car code) 'if))
                      (lets ((s1 t1 (lwalk walk `((_begin (,(caddr code))))  stack types))
                             (s2 t2 (lwalk walk `((_begin (,(cadddr code)))) stack types)))
                        ;; (print "s1: " s1 ", t1: " t1)
                        ;; (print "s2: " s2 ", t2: " t2)
                        (cond
                         ((and (= (len s1) (len s2)) (= (len s1) (len stack))) ; void
                          (values stack types))
                         ((and (= (len s1) (len s2)) (eq? (car s1) (car s2)))
                          (values s1 types)) ; same return type
                         ((= (len s1) (len s2))
                          (values (cons '(Any . |if expression|) stack) types))
                         (else
                          (error* `(,(format #f "Type mismatch in if expression inside of body of function `~a'." name)
                                                ',code
                                                "Both then and else must return a value or not return a value."
                                    ,(format #f "Here, `then' path yields the type stack of `~a'" s1)
                                    ,(format #f "While `else' path yields the type stack of `~a'" s2)
                                    "Note: if your function does not return a value (type: Void), You must explicitly declare that with a signature."))))))
                     ((and (list? code) (eq? (car code) '_with-label))
                      (lwalk walk `((_begin ,(caddr code))) stack types))
                     ((and (list? code) (eq? (car code) '_with-locals!))
                      (let* ((locals (reverse (cadr code)))
                             (n (len locals)))
                        (when (< (len stack) n)
                          (error* `(,(format #f "Cannot allocate locals `~a' in expression ~a." locals code)
                                    ,(format #f "Type stack: ~a" stack)
                                    ,(format #f "     Types: ~a" (ff->list types)))))
                        (lwalk
                         walk
                         (caddr code)
                         (drop stack n)
                         (fold
                          (λ (a b)
                            (let ((v (car b))
                                  (k (cdr b)))
                              (put a k v)))
                          types
                          (zip cons (take stack n) locals)))))
                     ((and (list? code) (has? special (car code)))
                      (values stack types))
                     ((and (list? code) (has? '(_tailcall! _tailcall-fast!) (car code))) ; pass typecheck as a normal funcall
                      (walk stack types `(,(cadr code) ,@(caddr code))))
                     ((list? code)
                      (lets ((stack types (walk stack types `(_begin ,(cdr code)))))
                        ;; TODO: typecheck if it's really calling a function
                        (when (not (get (get env 'labels empty) (car code) #f))
                          (skip `("cannot typecheck local function " ,code)))
                        (lets ((T (typecheck-funcall
                                   code
                                   (and (imm? (car code)) (car code))
                                   (reverse (take stack (len (cdr code)))) env error*))
                               (tcheckd (get env 'tcheckd empty))
                               (args (reverse (take stack (len (cdr code)))))
                               (typechecked? (has? (get tcheckd (car code) #n) args))
                               (env (put env 'tcheckd (put tcheckd (car code)
                                                           (cons args (get tcheckd (car code) #n))))))
                               ;; (T (if (and (not T) (eq? (car code) name)) 'Any T))) ; <- TODO: this sucks, we assume that un-declared recursive functions return something by default. if you want a Void, you have to be explicit
                          (if-lets ((defun (and (imm? (car code)) (not typechecked?) (name->defun (car code)))))
                            (call/cc
                             (λ (skip)
                               (walk-typecheck
                                (name->defun (car code))
                                (put env 'tcheck
                                     (put (get env 'tcheck empty)
                                          (car code)
                                          (ff 'args (reverse (take stack (len (cdr code))))
                                              'result (get (get (get env 'tcheck empty) (car code) empty) 'result 'Any))))
                                skip
                                name->defun
                                (λ (l) (error* (append l `(,(format #f "Called in ~a as `~a'" name code)))))
                                (+ depth 1)))))
                          ;; (print "T of " code " is " T)
                          (let ((S (drop stack (len (cdr code)))))
                            (values
                             (if T (cons (cons T '|...code|) S) S)
                             types)))))
                     (else
                      (error* "WTF (what that funcall):" code))))))
          (lets ((stack types (lwalk walk body #n (if argT
                                                      (list->ff (zip cons args
                                                                     (map
                                                                      (λ (a) (if (pair? a) a (cons a '__from-type)))
                                                                      argT)))
                                                      empty))))
            ;; (when (> (len stack) 1)
            ;;   (warn (format #f "Function `~a' leving stack with more than one value. Type stack: ~a" name stack)))
            ;; (print "stack on return of " name " is " stack)
            (let ((T (if (null? stack) 'Void (caar stack))))
              (if-lets ((resT resT))
                (if (is-of-type? T resT env)
                    env
                    (error* `(,(format #f "Type mismatch in return value of function `~a'." name)
                              "Function declared as"
                              ,(format #f "  ~a :: ~a" name (types->declaration (append (map car argT) (list resT))))
                              ,(format #f "With invalid return value deduced to be of type ~a" T)
                              ,(format #f "Which cannot be treated as ~a" resT))))
                (let ((ff (pipe empty
                            (put 'args (make-list (len args) '(Any . __from-type)))
                            (put 'result T)
                            (put 'inferred #t))))
                  ;; (print "inferred type of " name)
                  (put env 'tcheck (put (get env 'tcheck empty) name ff)))))))))

    (define (check-all-types-known env)
      (let ((tcheck (get env 'tcheck empty))
            (trules (get env 'trules empty))
            (types  (get env 'types #n)))
        (let loop ((t (keys tcheck)))
          (when (not (null? t))
            (let* ((ff (get tcheck (car t) empty))
                   (args (map car* (get ff 'args #n)))
                   (result (get ff 'result #f))
                   (all (append args (if result (list result) #n))))
              (let walk ((lst all))
                (when (not (null? lst))
                  (let ((it (car lst)))
                    (if (has? types it)
                        (walk (cdr lst))
                        (error (format "Unknown type `~a'." it)
                               "  in signature declaration:"
                               (format "(define-signature ~a ~a)" (car t) (types->declaration all)))))))
              (loop (cdr t)))))

        (let ((e (λ (t1 t2) (error (format "Unknown type `~a' in type rule for ~a." t1 t2)))))
          (let loop ((t (keys trules)))
            (when (not (null? t))
              (if (has? types (car t))
                  (begin
                    (let walk ((ts (keys (get trules (car t) empty))))
                      (when (not (null? ts))
                        (when (not (has? types (car ts)))
                          (e (car ts) (car t)))
                        (walk (cdr ts))))
                    (loop (cdr t)))
                  (e (car t) (car t))))))
        ))

    (define (typecheck code env)
      (check-all-types-known env)
      (let* ((defuns (code->defuns code))
             (name->defun (λ (name)
                            (let loop ((defuns defuns))
                              (if (null? defuns)
                                  #f
                                  (if (eq? (cadar defuns) name)
                                      (car defuns)
                                      (loop (cdr defuns)))))))
              (err (λ (l) (apply* error l))))
        (let loop ((defuns defuns) (env env))
          (if (null? defuns)
              env
              (begin
                (if-lets ((reason (call/cc (λ (skip) (walk-typecheck (car defuns) env skip name->defun err 0)))))
                  (begin
                    (when (and (verbose? env) (not (environment? reason)))
                      (format stdout "    [typecheck] skipped defun ~a because \"~a\"~%" (cadar defuns) reason))
                    (loop (cdr defuns) (if (environment? reason) reason env)))))))))
    ))
