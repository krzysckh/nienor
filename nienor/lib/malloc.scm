(define malloc/free-bit #b1)

(define (malloc/init)
  (let ((begin-size (- #xffff *compiler-end* 64))) ; lol
    (set! *compiler-end* (bior malloc/free-bit begin-size))))

(define (malloc/get-block-size ptr)
  (bior malloc/free-bit (bxor malloc/free-bit (bior malloc/free-bit (get! ptr)))))

(define (malloc/free? ptr)
  (equ? (band (get! ptr) malloc/free-bit) malloc/free-bit))

;; (alloc! _bs "(allocate-at) block-size: " 0)
;; (alloc! _ns "(allocate-at)  next-size: " 0)

(define (malloc/allocate-at ptr n)
  (let* ((block-size (malloc/get-block-size ptr))
         (next-size (signed- (signed- (signed block-size) 2) (signed (bior malloc/free-bit n))))
         (next-size (if (signed< next-size 0) 0 next-size)))
    ;; (puts-static _bs)
    ;; (print-number block-size)
    ;; (puts-static _ns)
    ;; (print-number next-size)
    (if (> next-size 3)
        (let ((next-ptr (+ ptr 2 n)))
          (set! next-ptr (bior next-size malloc/free-bit))
          (set! ptr (bxor malloc/free-bit (bior n malloc/free-bit))))
        (set! ptr (bxor malloc/free-bit (bior block-size malloc/free-bit))))
    (+ ptr 2)))

;; (alloc! _ma "_malloc: " 0)

(define (_malloc ptr n)
  ;; (puts-static _ma)
  ;; (print-number ptr)
  ;; (print-number n)
  (let ((size (malloc/get-block-size ptr)))
    (if (band (malloc/free? ptr) (>= size n))
        (malloc/allocate-at ptr n)
        (_malloc (+ ptr size 2) n))))

;; (alloc! _mal "(malloc) returning: " 0)
;; (alloc! _malr "(malloc) requested: " 0)

(define (malloc n)
  ;; (puts-static _malr)
  ;; (print-number n)
  (let ((v (_malloc *compiler-end* (if (equ? (band malloc/free-bit n) malloc/free-bit) n (+ 1 n)))))
    ;; (puts-static _mal)
    ;; (print-number v)
    v))

;; (alloc! _rf "(refs) is free: " 0)

(define (malloc/update-refs p)
  (if (< p *compiler-end*)
    (noop)
    (let ((siz (malloc/get-block-size p)))
      (when (and (malloc/free? p) (malloc/free? (+ p 2 siz)))
        ;; (puts-static _rf)
        ;; (print-number (+ p 2 siz))
        (set! p (bior malloc/free-bit (+ (malloc/get-block-size p) (malloc/get-block-size (+ p 2 siz)) 2))))
      (malloc/update-refs (+ (malloc/get-block-size p) 2)))))

(define (free ptr)
  (let ((p (- ptr 2)))
    (set! p (bior malloc/free-bit (get! p)))
    (malloc/update-refs *compiler-end*)))

(define-macro-rule ()
  (delete x)
  (free x))
