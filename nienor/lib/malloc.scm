(define malloc/free-bit #b1)

(define (malloc/init)
  (let ((begin-size (- #xffff *compiler-end* 64))) ; lol
    (set! *compiler-end* (bior malloc/free-bit begin-size))))

(define (malloc/get-block-size ptr)
  (bior malloc/free-bit (bxor malloc/free-bit (bior malloc/free-bit (get! ptr)))))

(define (malloc/free? ptr)
  (equ? (band (get! ptr) malloc/free-bit) malloc/free-bit))

(define (malloc/allocate-at ptr n)
  (let* ((block-size (malloc/get-block-size ptr))
         (next-size (- block-size 2 (bior malloc/free-bit n))))
    (if (> next-size 3)
        (let ((next-ptr (+ ptr 2 n)))
          (set! next-ptr (bior next-size malloc/free-bit))
          (set! ptr (bxor malloc/free-bit (bior n malloc/free-bit))))
        (set! ptr (bxor malloc/free-bit (bior block-size malloc/free-bit))))
    (+ ptr 2)))

(define (_malloc ptr n)
  (let ((size (malloc/get-block-size ptr)))
    (if (band (malloc/free? ptr) (>= size n))
        (malloc/allocate-at ptr n)
        (_malloc (+ ptr size 2) n))))

(define (malloc n)
  (let ((v (_malloc *compiler-end* (if (equ? (band malloc/free-bit n) malloc/free-bit) n (+ 1 n)))))
    v))

(define (malloc/update-refs p)
  (if (< p *compiler-end*)
    (noop)
    (let ((siz (malloc/get-block-size p)))
      (when (malloc/free? (+ p 2 siz))
        (set! p (bior malloc/free-bit (+ (get! p) (+ (malloc/get-block-size (+ p 2 siz)) 2)))))
      (malloc/update-refs (+ (malloc/get-block-size p) 2)))))

(define (free ptr)
  (let ((p (- ptr 2)))
    (set! p (bior malloc/free-bit (get! p)))
    (malloc/update-refs *compiler-end*)))

(define-macro-rule ()
  (delete x)
  (free x))
