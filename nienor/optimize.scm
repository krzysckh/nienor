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
                (else
                 (if (eq? (car* e) name)
                     `(_tailcall! ,name ,(cdr e) ,n-locals)
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

    (define number?-a (make-pred number? 'a))
    (define number?-b (make-pred number? 'b))
    (define (make-folder pred)
      (λ (vs)
        (pred (cdr (assoc 'b vs)) (cdr (assoc 'a vs)))))

    (define constant-folders
      ;; literal                 match                                              replace
      `(((nigeb uxn-call! add 2) (nigeb (uxn-call! (2) add) ,number?-a ,number?-b) ,(make-folder +))
        ((nigeb uxn-call! sub 2) (nigeb (uxn-call! (2) sub) ,number?-a ,number?-b) ,(make-folder -))
        ((nigeb uxn-call! mul 2) (nigeb (uxn-call! (2) mul) ,number?-a ,number?-b) ,(make-folder *))
        ((nigeb uxn-call! div 2) (nigeb (uxn-call! (2) div) ,number?-a ,number?-b) ,(make-folder (λ (a b) (floor (/ a b)))))
        ))

    (define (fold-constants lst)
      (apply-macros
       (fold
        (λ (a b)
          (add-macro a (cadr b) (caddr b) (car b)))
        empty-env
        constant-folders)
       lst))

    (define (optimize lst verbose?)
      (lets ((lst (keep-only-used-defuns lst verbose?)) ; delete unused defuns that would eat up space
             (lst (find-tailcalls lst))                 ; add TCO marks
             (lst (fold-constants lst))                 ; fold constants
             )
        lst))
    ))
