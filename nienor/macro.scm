#| doc
a simple macro system for nienor. nothing fancy, just enough to get the lisp going
|#
(define-library (nienor macro)
  (import
   (owl toplevel)
   (owl proof)
   (nienor common))

  (export
   macro-matches?
   rewrite-macro)

  (begin
    (define atom? (B not pair?))

    (define (macro-matches? macro exp literal)
      (cond
       ((and (null? macro) (null? exp)) '())
       ((and (not (pair? macro)) (list? exp))
        `((,macro . ,exp))) ; (a b . c)
       ((has? literal (car* macro))
        (if (eq? (car* macro) (car* exp))
            (macro-matches? (cdr* macro) (cdr* exp) literal)
            #f))
       ((and (pair? (car* macro)) (pair? (car* exp)) (not (null? exp)))
        (if-lets ((a (macro-matches? (car* macro) (car* exp) literal))
                  (b (macro-matches? (cdr* macro) (cdr* exp) literal)))
          (append a b)
          #f))
       ((and (atom? (car* macro)) (not (null? (car* exp))))
        (if-lets ((a (macro-matches? (cdr* macro) (cdr* exp) literal)))
          (append `((,(car* macro) . ,(car* exp))) a)
          #f))
       (else
        #f)))

    (define (rewrite-macro macro rewrite-rule exp literal)
      (let ((rewrite (macro-matches? macro exp literal)))
        (let walk ((exp rewrite-rule))
          (cond
           ((null? exp) #n)
           ((pair? (car* exp))
            (append
             (list (walk (car exp)))
             (walk (cdr exp))))
           ((not (pair? exp)) ; ilist = (a b . c)
            (if-lets ((val (cdr* (assoc exp rewrite))))
              val
              (error "invalid dot" exp "in macro" macro)))
           (else
            (if-lets ((val (cdr* (assoc (car exp) rewrite))))
              (cons val (walk (cdr exp)))
              (cons (car exp) (walk (cdr exp)))))))))

    (define (walk-ok lst)
      (if (null? lst)
          #t
          (if (eq? (car lst) #t)
              (walk-ok (cdr lst))
              (car lst))))

    (define tests-1
      '(
        ((f a b c)        (func 1 2 3) () #t)
        ((f (a b) c)      (func (1 2) 3) () #t)
        ((f (((a) b) c))  (func (((1) 2) 3)) () #t)
        ((f a b . c)      (func 1 2 3) () #t)
        ((f a b . c)      (func 1 2 3 4) () #t)
        ((f a b . c)      (func 1 2) () #t)
        ((f a => b)        (func a b c)   (=>) #f)
        ((f a => b)        (func a => c)  (=>) #t)
        ((f (_ b . c) . a) (func (_) a b) (_) #f)))

    (define (tests-1-ok?)
      (walk-ok
       (map
        (λ (test)
          (lets ((macro (lref test 0))
                 (exp (lref test 1))
                 (literal (lref test 2))
                 (expected-result (lref test 3)))
            (if (if expected-result
                    (macro-matches? macro exp literal)
                    (not (macro-matches? macro exp literal)))
                #t
                (tuple "fail" macro))))
        tests-1)))

    (define tests-2
      '(((f a b c)       (f a)       (func 1 2 3)       (func 1))
        ((f (a b) c)     (f b)       (func (1 2) 3)     (func 2))
        ((f (((a) b) c)) (f (b a) c) (func (((1) 2) 3)) (func (2 1) 3))
        ((f a b . c)     (f c)       (func 1 2 3)       (func (3)))
        ((f a b . c)     (f . c)     (func 1 2 3)       (func 3))
        ((f a b . c)     (f c)       (func 1 2 3 4)     (func (3 4)))
        ((f a b . c)     (f . c)     (func 1 2 3 4)     (func 3 4))
        ((f (a . b) (c (d . e)))
         (f (c . b) ((d c) . e))
         (f1 (1 2 3 4) (f2 (f3 6 7 8)))
         (f1 (f2 2 3 4) ((f3 f2) 6 7 8)))))

    (define (tests-2-ok?)
      (walk-ok
       (map
        (λ (test)
          (lets ((macro           (lref test 0))
                 (rewrite         (lref test 1))
                 (exp             (lref test 2))
                 (expected-result (lref test 3))
                 (result          (rewrite-macro macro rewrite exp #n)))
            (if (equal? result expected-result)
                #t
                (tuple "fail" macro "got" result "expected" expected-result))))
        tests-2)))

    (example
     (tests-1-ok?) = #t
     (tests-2-ok?) = #t)

    ))
