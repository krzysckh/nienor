;; Memory layout after *compiler-end*:
;;
;; 0x.. 0x.. [data] 0x.. 0x.. [data]
;; \__HDR__/        \__HDR__/
;;   ↑                   ↑
;; header with the size of allocated block
;; if the lowest bit is set, the block is free, else it's used
;; it's then assumed that the low bit is always one, so a block might get allocated
;; 1 byte more than requested

;; TODO: implement closure generation with malloc to be able to free them

(define malloc/free-bit #b1)

(define (malloc/init)
  (let ((begin-size (- #xffff *compiler-end* 64))) ; lol
    (set! *compiler-end* (bior malloc/free-bit begin-size))))

(define (malloc/get-block-size ptr)
  (bior malloc/free-bit (bxor malloc/free-bit (bior malloc/free-bit (get! ptr)))))

(define (malloc/free? ptr)
  (equ? (band (get! ptr) malloc/free-bit) malloc/free-bit))

(define (malloc/allocate-at ptr n)
  (let ((block-size (malloc/get-block-size ptr))
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
  (_malloc *compiler-end* (if (equ? (band malloc/free-bit n) malloc/free-bit) n (+ 1 n))))

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

(define (main)
  (malloc/init)
  (print-number *compiler-end*)
  (let ((p1 (malloc 100))
        (p2 (malloc 100))
        (p3 (malloc 100))
        (p4 (malloc 100)))
    (print-number p1)
    (print-number p2)
    (print-number p3)
    (print-number p4)
    (free p1)
    (free p2)
    (print-number (malloc 200)))
  (exit!))
