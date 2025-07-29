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
    (define (types-of f env)
      (if-lets ((ff (get env 'tcheck #f))
                (v  (get ff f #f)))
        (values (get v 'args #n) (get v 'result #f))
        (if-lets ((a (get (get env 'arity empty) f #f)))
          (values (make-list a 'Any) 'Any)
          (values #f #f))))

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

    (define (imm->type v env)
      (cond
       ((boolean? v)           'Bool)
       ((number? v)            'Number)
       ((string? v)            'String)
       ((eq? (car* v) 'symbol) 'Symbol)
       ((symbol? v)            (if (eq? 'Any (func->type v env)) 'Any 'Pointer)) ; a function pointer OR a value TODO: variables
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

    (define (typecheck-funcall funcall types env)
      (lets ((f (car funcall))
             (args (cdr funcall))
             (argT resT (types-of f env)))
        (when argT
          (if (has? special f)
              #t
              (let* ((arg-types (zip cons args (map (λ (a) (get types a #f)) args)))
                     (arg-types (map
                                 (λ (c)
                                   (if (cdr c)
                                       c
                                       (cons (car c) (imm->type (car c) env))))
                                 arg-types)))
                (let ((l (map (λ (a) (cdr* (assoc a arg-types))) args)))
                  (when (get env 'verbose? #f)
                    (format stdout "    [typecheck] ~a requires '~a, gets '~a~%" funcall argT l))
                  (let loop ((required argT) (got l))
                    (cond
                     ((and (null? got) (null? required))
                      #t)
                     ((or (null? got) (null? required))
                      (error (format #f "Invalid arity in function call `~a' — expected ~a, got ~a" funcall argT l)
                             "Function declared as"
                             (format #f "  ~a :: ~a" f (types->declaration (append argT (list resT))))
                             "Used as"
                             (format #f "  ~a :: ~a" f (types->declaration (append l (list resT))))))
                     (else
                      (if (or (eq? (car required) 'Any)
                              (has? (assoc (car got) association-table) (car required)))
                          (loop (cdr required) (cdr got))
                          (error (format #f "Mismatched types in function call `~a' — expected ~a, got ~a" funcall argT l)
                                 "Function declared as"
                                 (format #f "  ~a :: ~a" f (types->declaration (append argT (list resT))))
                                 "Used as"
                                 (format #f "  ~a :: ~a" f (types->declaration (append l (list resT))))
                                 (format #f "~a cannot be treated as ~a" (car got) (car required)))
                          ))))))))))

    (define (walk-typecheck defun env)
      (lets ((name (cadr   defun))
             (args (caddr  defun))
             (body (cadddr defun))
             (argT resT (types-of name env)))
        (letrec ((walk
                  (λ (types code)
                    (cond
                     ((null? code) types)
                     ((imm? code)  types)
                     ((and (list? code) (eq? (car code) '_begin))
                      (for-each (H walk types) (cadr code))
                      types)
                     ((and (list? code) (eq? (car code) 'if))
                      (for-each (H walk types) (cdr code))
                      types)
                     ((and (list? code) (eq? (car code) '_with-locals!))
                      (for-each (H walk types) (cadr code))
                      types)
                     ((and (list? code) (has? special (car code)))
                      types)
                     ((and (list? code) (has? '(_tailcall! _tailcall-fast!) (car code)))
                      (typecheck-funcall (append (list (cadr code)) (caddr code)) types env)
                      (for-each (H walk types) (caddr code))
                      types)
                     ((list? code)
                      (typecheck-funcall code types env)
                      (for-each (H walk types) (cdr code))
                      types)
                     (else
                      (error "WTF (what that funcall):" code))))))
          (fold walk (if argT (list->ff (zip cons args argT)) empty) body))))

    (define (typecheck code env)
      (let ((defuns (code->defuns code))
            (walk (C walk-typecheck env)))
        (for-each walk defuns)))

    ))
