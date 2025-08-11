(define-library (nienor optimize)
  (import
   (owl toplevel)
   (nienor common)
   (nienor macro))

  (export
   code->defuns
   code->used-symbols
   optimize)

  (begin
    (define (uniq lst)
      (let loop ((lst lst) (acc #n))
        (cond
         ((null? lst) acc)
         ((has? acc (car lst)) (loop (cdr lst) acc))
         (else
          (loop (cdr lst) (cons (car lst) acc))))))

    (define *symbols-used-internally* '(main malloc/init malloc *METADATA-FINISH*))

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

    (define (find-used-in-allocs lst)
      (fold
       (λ (a b)
         (append a (filter symbol? (caddr b))))
       #n
       (filter
        (λ (e) (eq? (car* e) '_alloc!))
        lst)))

    ;; This function looks from main and tries to find all functions that _are_ used
    (define (find-used-from name lst)
      (if-lets ((root (find-defun lst name)))
        (let loop ((seen (list name)) (syms (defun->used-symbols root)))
          (if (null? syms)
              seen
              (if-lets ((fun (find-defun lst (car syms))))
                (loop (cons (car syms) seen)
                      (append (cdr syms) (filter
                                          (λ (s) (not (has? seen s)))
                                          (defun->used-symbols fun))))
                (loop (cons (car syms) seen) (cdr syms)))))
        #n))

    (define (find-used lst)
      (append
       (find-used-from 'main lst)
       (fold
        (λ (a b)
          (append a (find-used-from b lst)))
        #n
        (uniq (find-used-in-allocs lst)))))

    ;; this removes unused defuns, defun-vectors, _alloc!s and nalloc!s
    (define (keep-only-used-defuns lst verbose? keep)
      (if-lets ((used (append keep (find-used lst))))
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

    (define (maybe-tailcall defun defun?)
      (let ((name (cadr defun))
            (nargs (len (caddr defun)))
            (code (cadddr defun)))
        `(_defun
          ,name ,(caddr defun)
          ,(append
            (but-last code)
            (list
             (let loop ((e (last code #f))
                        (n-locals 0))
               (cond
                ((eq? (car* e) '_begin)
                 (if (null? (cadr e))
                     `(_begin ())
                     `(_begin (,@(but-last (cadr e)) ,(loop (last (cadr e) #n) n-locals)))))
                ((eq? (car* e) '_with-locals!)
                 `(_with-locals! ,(cadr e) (,@(but-last (caddr e)) ,(loop (last (caddr e) #n) (+ n-locals (len (cadr e)))))))
                ((eq? (car* e) 'if)
                 `(if ,(cadr e) ,(loop (caddr e) n-locals) ,(loop (cadddr e) n-locals)))
                ((eq? (car* e) name)
                 `(_tailcall-fast! ,(car e) ,(cdr e) ,n-locals))
                ((and (list? e) (if (symbol? (car e)) (defun? (car e)) #t))
                 `(_tailcall! ,(car e) ,(cdr e) ,(+ nargs n-locals)))
                (else
                 e))))))))

    (define (code->defuns lst)
      (filter (λ (l) (or (eq? (car* (car* l)) '_defun)
                         (eq? (car* (car* l)) '_defun-vector)))
              lst))

    (define (make-defun? lst)
      (let ((defuns (map (B car* cdr*) (code->defuns lst))))
        (λ (sym)
          (has? defuns sym))))

    (define (find-tailcalls lst)
      (let ((defun? (make-defun? lst)))
        (let loop ((lst lst) (acc #n))
          (let ((exp (car* lst)))
            (cond
             ((null? lst) acc)
             ((eq? (car* exp) '_defun)
              (let ((mt (maybe-tailcall exp defun?)))
                (loop (cdr lst) (append acc (list mt)))))
             (else
              (loop (cdr lst) (append acc (list exp)))))))))

    (define (_imm? x) (and (not (symbol? x)) (imm? x)))

    (define number?-a (make-pred number? 'a))
    (define number?-b (make-pred number? 'b))

    (define imm?-a (make-pred imm? 'a))
    (define imm?-b (make-pred imm? 'b))

    (define _imm?-a (make-pred _imm? 'a))
    (define _imm?-b (make-pred _imm? 'b))

    (define (make-folder pred)
      (λ (vs)
        (pred (cdr (assoc 'b vs)) (cdr (assoc 'a vs)))))

    ;; TODO: all these rely too heavily on prelude implemntation
    (define constant-folders
      ;; literal                 match                                              replace
      `(((_begin uxn-call! add 2) (_begin (,number?-b ,number?-a (uxn-call! (2) add))) ,(make-folder +))
        ((_begin uxn-call! sub 2) (_begin (,number?-b ,number?-a (uxn-call! (2) sub))) ,(make-folder -))
        ((_begin uxn-call! mul 2) (_begin (,number?-b ,number?-a (uxn-call! (2) mul))) ,(make-folder *))
        ((_begin uxn-call! ora 2) (_begin (,number?-b ,number?-a (uxn-call! (2) ora))) ,(make-folder bior))
        ((_begin uxn-call! eor 2) (_begin (,number?-b ,number?-a (uxn-call! (2) eor))) ,(make-folder bxor))
        ((_begin uxn-call! and 2) (_begin (,number?-b ,number?-a (uxn-call! (2) and))) ,(make-folder band))
        ((_begin uxn-call! div 2) (_begin (,number?-b ,number?-a (uxn-call! (2) div))) ,(make-folder (λ (a b) (floor (/ a b)))))
        ((equ?)                   (equ? ,imm?-a ,imm?-b)                               ,(make-folder eqv?))
        ((<) (< ,number?-b ,number?-a) ,(λ (vs) (if ((make-folder <) vs) 1 0)))
        ((>) (> ,number?-b ,number?-a) ,(λ (vs) (if ((make-folder >) vs) 1 0)))
        ((if) (if ,_imm?-a then else) ,(λ (vs) ; compiler-if :P
                                         (let ((v (cdr (assoc 'a vs))))
                                           (if (if (number? v) (= 0 v) (eqv? #f v))
                                               (cdr (assoc 'else vs))
                                               (cdr (assoc 'then vs))))))
        ((>>) (>> ,number?-a ,number?-b) ,(make-folder (λ (b a) (band #xffff (>> a b)))))
        ((<<) (<< ,number?-a ,number?-b) ,(make-folder (λ (b a) (band #xffff (<< a b)))))
        ))

    (define (fold-constants lst)
      (apply-macros (add-macros add-macro constant-folders empty-env) lst))

    (define-syntax pipe-list
      (syntax-rules ()
        ((_ l (exp1 . rest) . body)
         ((λ (l)
           (if (list? l)
               (_ l rest . body)
               l))
          exp1))
        ((_ . body)
         (begin . body))
        ))

    ;; keep = list of symbols that are unresolved & are needed to keep as they might be not referenced in code
    (define (optimize lst verbose? keep)
      (pipe-list lst
                ((fold-constants lst)                      ; fold constants
                 (keep-only-used-defuns lst verbose? keep) ; delete unused defuns that would eat up space
                 (find-tailcalls lst))                     ; add TCO/TCE marks
         lst))
    ))
