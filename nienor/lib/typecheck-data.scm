;; this is fucking hilarious
;; _symbol is not a funcion, but the typechecker will treat it as such
(define-signature _symbol Any -> Symbol)

(define-macro-rule ()
  (typechecker-bogger-off! . rest)
  (_typechecker-bogger-off! rest))

(define-macro-rule ()
  (define-type T)
  (_define-type T))

(define-macro-rule ()
  (define-types T . rest)
  (flatten!
   (define-type T)
   (define-types . rest)))

(define-macro-rule ()
  (define-types T)
  (define-type T))

;; Defining basic types
(define-types Number String Symbol Bool Pointer Any Void)

(define-macro-rule ()
  (define-typing-rules rule . rules)
  (flatten!
   (define-typing-rule . rule)
   (define-typing-rules . rules)))

(define-macro-rule ()
  (define-typing-rules)
  (flatten!))

(define-macro-rule (is <=>)
  (define-typing-rule T1 binding is T2 <=> exp)
  (_define-typing-rule T1 T2 binding exp))

(define-macro-rule (is <=>)
  (define-typing-rule T1 is T2)
  (_define-typing-rule T1 T2 _ #t))

;; basic association table
(define-typing-rules
  (Symbol  is Number)
  (Symbol  is Bool)

  (Bool    is Number)

  (Number  is Pointer)
  (Number  is Bool)

  (Pointer is Number)
  (Pointer is Bool)

  (String  is Pointer))
