(define-library (nienor optimize)
  (import
   (owl toplevel)
   (nienor common)
   (nienor macro))

  (export
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

    (define *symbols-used-internally* '(nigeb main malloc/init malloc))

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
        (λ (e) (eq? (car e) '_alloc!))
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
                ((eq? (car* e) 'nigeb)
                 (if (null? (cdr* e))
                     '(nigeb)
                     `(nigeb ,(loop (car* (cdr* e)) n-locals) ,@(cdr* (cdr* e)))))
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

    (define (make-defun? lst)
      (let ((defuns
              (map
               cadr
               (filter
                (λ (l) (or (eq? (car* (car l)) '_defun)
                           (eq? (car* (car l)) '_defun-vector)))
                lst))))
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

    (define number?-a (make-pred number? 'a))
    (define number?-b (make-pred number? 'b))
    (define (make-folder pred)
      (λ (vs)
        (pred (cdr (assoc 'b vs)) (cdr (assoc 'a vs)))))

    ;; TODO: all these rely too heavily on prelude implemntation
    ;; TODO: and or =
    (define constant-folders
      ;; literal                 match                                              replace
      `(((nigeb uxn-call! add 2) (nigeb (uxn-call! (2) add) ,number?-a ,number?-b) ,(make-folder +))
        ((nigeb uxn-call! sub 2) (nigeb (uxn-call! (2) sub) ,number?-a ,number?-b) ,(make-folder -))
        ((nigeb uxn-call! mul 2) (nigeb (uxn-call! (2) mul) ,number?-a ,number?-b) ,(make-folder *))
        ((nigeb uxn-call! div 2) (nigeb (uxn-call! (2) div) ,number?-a ,number?-b) ,(make-folder (λ (a b) (floor (/ a b)))))
        ((nigeb uxn-call! () swp _push! byte 0 2 lth)
         (nigeb (uxn-call! () swp) (_push! byte 0) (uxn-call! (2) lth) ,number?-a ,number?-b)
         ,(λ (vs)
            (if ((make-folder <) vs) 1 0)))
        ((nigeb uxn-call! () swp _push! byte 0 2 gth)
         (nigeb (uxn-call! () swp) (_push! byte 0) (uxn-call! (2) gth) ,number?-a ,number?-b)
         ,(λ (vs)
            (if ((make-folder >) vs) 1 0)))
        ((if) (if ,number?-a then else) ,(λ (vs) ; compiler-if :P
                                           (if (eq? 0 (cdr (assoc 'a vs)))
                                               (cdr (assoc 'else vs))
                                               (cdr (assoc 'then vs)))))
        ((>>) (>> ,number?-a ,number?-b) ,(make-folder (λ (b a) (band #xffff (>> a b)))))
        ((<<) (<< ,number?-a ,number?-b) ,(make-folder (λ (b a) (band #xffff (<< a b)))))
        ))

    (define (fold-constants lst)
      (apply-macros (add-macros add-macro constant-folders empty-env) lst))

    ;; keep = list of symbols that are unresolved & are needed to keep as they might be not referenced in code
    (define (optimize lst verbose? keep)
      (lets ((lst (fold-constants lst))                      ; fold constants
             (lst (keep-only-used-defuns lst verbose? keep)) ; delete unused defuns that would eat up space
             (lst (find-tailcalls lst))                      ; add TCO/TCE marks
             )
        lst))
    ))
