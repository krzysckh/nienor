(define-library (nienor typecheck)
  (import
   (owl toplevel)
   (nienor common)
   (nienor optimize)
   (nienor macro))

  (export
   types-of
   typecheck)

  (begin
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

    (define (types-of f env)
      (if f
          (if-lets ((ff (get env 'tcheck #f))
                    (v  (get ff f #f)))
            (values (get v 'args #n) (get v 'result #f))
            (if-lets ((a (get (get env 'arity empty) f #f)))
              (values (make-list a 'Any) 'Any)
              (values #f #f)))
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
       ((boolean? v)           'Bool)
       ((number? v)            'Number)
       ((string? v)            'String)
       ((eq? (car* v) 'symbol) 'Symbol)
       ((symbol? v)            (if-lets ((r (get types v #f)))
                                 r
                                 (if (eq? 'Any (func->type v env))
                                     'Any
                                     'Pointer))) ; a function pointer OR a value TODO: variables, TODO: actual symbols
       ((list? v)              (funcall->type v env))
       (else
        #f)))

    (define association-table
      ;; this     is also this
      `((Symbol   Number Bool)
        (Bool     Number)
        (Number   Pointer Bool)
        (Pointer  Number Bool)
        (String   Pointer)
        (Void)
        (Any      Number Pointer String Symbol)))

    (define imm? (B not pair?))

    (define special '(_begin uxn-call! _push!))

    (define (types->declaration l)
      (if (= (len l) 1)
          (car l)
          (fold
           (λ (a b) (str a " -> " b))
           (car* l)
           (cdr* l))))

    (define (typecheck-funcall funcall fun-name arg-types env)
      ;; (print "typecheck-funcall " funcall ": "  fun-name " " arg-types)
      (if-lets ((argT resT (types-of fun-name env)))
        (if (has? special fun-name)
            (if (verbose? env)
                (begin (format stdout "    [typecheck] cannot typecheck `~a' — special funcall" funcall) #t)
                #t)
            (begin
              (when (verbose? env)
                (format stdout "    [typecheck] ~a requires '~a, gets '~a~%" funcall argT arg-types))
              (let loop ((required argT) (got arg-types))
                (cond
                 ((and (null? got) (null? required))
                  (if (eq? 'Void resT) #f resT))
                 ((or (null? got) (null? required))
                  (error (format #f "Invalid arity in function call `~a' — expected ~a, got ~a" funcall argT arg-types)
                         "Function declared as"
                         (format #f "  ~a :: ~a" fun-name (types->declaration (append argT (list resT))))
                         "Used as"
                         (format #f "  ~a :: ~a" fun-name (types->declaration (append arg-types (list resT))))))
                 (else
                  (if (or (eq? (car required) 'Any)
                          (has? (assoc (car got) association-table) (car required)))
                      (loop (cdr required) (cdr got))
                      (error (format #f "Mismatched types in function call `~a' — expected ~a, got ~a" funcall argT arg-types)
                             "Function declared as"
                             (format #f "  ~a :: ~a" fun-name (types->declaration (append argT (list resT))))
                             "Used as"
                             (format #f "  ~a :: ~a" fun-name (types->declaration (append arg-types (list resT))))
                             (format #f "~a cannot be treated as ~a" (car got) (car required)))
                      ))))))))

    (define (lwalk walk l stack types)
      (let loop ((stack stack) (types types) (l l))
        (if (null? l)
            (values stack types)
            (lets ((stack types (walk stack types (car l))))
              (loop stack types (cdr l))))))

    (define (walk-typecheck defun env skip)
      ;; (print "walk typecheck " defun)
      (lets ((name (cadr   defun))
             (args (caddr  defun))
             (body (cadddr defun))
             (argT resT (types-of name env)))
        ;; TODO: report function name for easier searching
        (letrec ((walk
                  (λ (stack types code)
                    ;; (print "[typecheck] code: " code)
                    ;; (print "            stack: " stack)
                    ;; (print "            types: " (ff->list types))

                    (cond
                     ((null? code) (values stack types))
                     ((imm? code)  (values (cons (imm->type code env types) stack) types))
                     ((and (list? code) (eq? (car code) 'uxn-call!))
                      ;; (print 'yup)
                      (if (equal? '(2) (cadr code))
                          (if-lets ((info (get *effect-table* (caddr code) #f))
                                    (takes (car info))
                                    (adds  (cadr info)))
                            (values
                             (append (make-list adds 'Any) (drop stack takes))
                             types)
                            (skip "not found in effect table"))
                          (skip "bad uxn-call")))
                     ((and (list? code) (eq? (car code) '_push!))
                      (when (eq? (cadr code) 'byte)
                        (skip "byte _push!"))
                      (values (cons (imm->type (caddr code) env types) stack) types))
                     ((and (list? code) (eq? (car code) '_begin))
                      (lwalk walk (cadr code) stack types))
                     ((and (list? code) (eq? (car code) 'if)) ; TODO: this is false
                      (lwalk walk `(_begin (,(cadr code) ,(caddr code)))  stack types))
                     ((and (list? code) (eq? (car code) '_with-locals!))
                      (let* ((locals (reverse (cadr code)))
                             (n (len locals)))
                        (when (< (len stack) n)
                          (error (format #f "Cannot allocate locals `~a' in expression ~a." locals code)
                                 (format #f "Type stack: ~a" stack)
                                 (format #f "     Types: ~a" (ff->list types))))
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
                     ((and (list? code) (has? '(_tailcall! _tailcall-fast!) (car code)))
                      (lets ((stack types (walk stack types `(_begin ,(reverse (caddr code))))))
                        ;; TODO: typecheck if it's really calling a function
                        (values
                         (if-lets ((T (typecheck-funcall `(,(cadr code) ,@(caddr code)) (cadr code) (take stack (len (caddr code))) env)))
                           (cons T stack)
                           stack)
                         types)))
                     ((list? code)
                      (lets ((stack types (walk stack types `(_begin ,(reverse (cdr code))))))
                        ;; TODO: typecheck if it's really calling a function
                        (values
                         (if-lets ((T (typecheck-funcall code
                                                         (and (imm? (car code)) (car code))
                                                         (take stack (len (cdr code))) env)))
                           (cons T stack)
                           stack)
                         types)))
                     (else
                      (error "WTF (what that funcall):" code))))))
          (lets ((_1 _2 (lwalk walk body #n (if argT (list->ff (zip cons args argT)) empty))))
            'ok))))

    (define (typecheck code env)
      (let ((defuns (code->defuns code))
            (walk (λ (code) (walk-typecheck code env))))
        (let loop ((defuns defuns))
          (if (null? defuns)
              #t
              (begin
                (call/cc (λ (skip) (walk-typecheck (car defuns) env skip)))
                ;; (walk-typecheck (car defuns) env self)
                (loop (cdr defuns)))))))
    ))
