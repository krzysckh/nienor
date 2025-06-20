;;; hello i re-wrote malloc as the old one literally did not fucking work

(define malloc/margin 16)

;; memory layout:
;;   [ ] [    ] ...............
;; ↑ data size  allocated data
;; | byte short
;; end of rom
;;
;; data byte: #b00000001
;;              \_____/|
;;   fucking unused    free flag

(define (malloc/init)
  (let ((begin-size (- (- #xffff *compiler-end*) malloc/margin))) ; lol
    (set8! *compiler-end* #b1)
    (set! (+ *compiler-end* 1) (- (- #xffff *compiler-end*) malloc/margin))))

;; real ptr → block size
(define (malloc/get-block-size ptr)
  (get! (- ptr 2)))

;; real ptr → free?
(define (malloc/free? ptr)
  (get8! (- ptr 3)))

;; mark real ptr as used
(define (malloc/set-used! ptr)
  (set8! (- ptr 3) (bxor #b1 (bior #b1 (get8! (- ptr 3))))))

;; mark real ptr as free
(define (malloc/set-free! ptr)
  (set8! (- ptr 3) (bior #b1 (get8! (- ptr 3)))))

;; update size of real ptr
(define (malloc/set-size! ptr sz)
  (set! (- ptr 2) sz))

(define (malloc/create-block-after ptr sz)
  (let ((base (+ 3 ptr (malloc/get-block-size ptr))))
    (malloc/set-free! base)
    (malloc/set-size! base sz)))

(define (malloc n)
  (_malloc n (+ 3 *compiler-end*)))

(define (_malloc n ptr)
  (if (and (malloc/free? ptr) (>= (malloc/get-block-size ptr) n))
      (let ((rest-size (- (malloc/get-block-size ptr) n)))
        (if (> rest-size 4) ; we can insert another block later
            (begin
              (malloc/set-used! ptr)
              (malloc/set-size! ptr n)
              (malloc/create-block-after ptr (- rest-size 3))
              )
            (begin
              (malloc/set-used! ptr)))
        ptr)
      (_malloc n (+ ptr (malloc/get-block-size ptr) 3))))

(define (malloc/next-block ptr)
  (+ ptr (malloc/get-block-size ptr) 3))

(define (malloc/connect-refs! ptr)
  (if (and (<= ptr (- #xffff malloc/margin)) (>= ptr *compiler-end*))
      (let ((next (malloc/next-block ptr)))
        (if (and (malloc/free? ptr) (malloc/free? next))
            (begin
              (malloc/set-size! ptr (+ (malloc/get-block-size ptr) (malloc/get-block-size next) 3))
              (malloc/connect-refs! ptr))
            (malloc/connect-refs! (malloc/next-block ptr))))
      (noop)))

(define (free ptr)
  (malloc/set-free! ptr)
  (malloc/connect-refs! (+ 3 *compiler-end*)))

(define-macro-rule ()
  (delete x)
  (free x))
