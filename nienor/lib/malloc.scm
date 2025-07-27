;;; hello i re-wrote malloc as the old one literally did not fucking work

(define malloc/margin 16)

;;; WARNING
;; You have to be really, really careful when using memory spat out by malloc.
;; Do not ever try writing outside of mallocated bounds. This will make it explode. Future allocations will be impossible.
;;
;; Example of what *not* to do:
;;
;;   (let ((p (malloc 2)))
;;     (memcpy p from-somewhere 4)) ; <- here we write overwrite 2 bytes of internal data
;;
;; Because of how dead simple this allocator is, You have to be really careful.
;; You have been warned.

;; memory layout:
;;   #xXX #xYY #xZZ ........................ (NO PADDING)   \
;; ↑ \__/ \_______/ \______________________/ \__________/   |
;; | |    |          [#xYYZZ of user data]     [0 bytes   ] | repeats until #xffff - malloc/margin
;; | |    [size short]                         [of padding] |
;; | [data byte]                                            |
;; end of rom                                               /
;;
;; please do note the "NO PADDING" between an allocation & start of important private data of next cell.
;; don't do off by one errors
;;
;; data byte: #b00000001
;;              \_____/↑
;;   fucking unused    free flag

(define-signature malloc/init Void)
(define (malloc/init)
  (let ((begin-size (- (- #xffff *compiler-end*) malloc/margin))) ; lol
    (set8! *compiler-end* #b1)
    (set! (+ *compiler-end* 1) (- (- #xffff *compiler-end*) malloc/margin))))

;; real ptr → block size
(define-signature malloc/get-block-size Pointer -> Number)
(define (malloc/get-block-size ptr)
  (get! (- ptr 2)))

;; real ptr → free?
(define-signature malloc/free? Pointer -> Bool)
(define (malloc/free? ptr)
  (get8! (- ptr 3)))

;; mark real ptr as used
(define-signature malloc/set-used! Pointer -> Void)
(define (malloc/set-used! ptr)
  (set8! (- ptr 3) (band (bnot #b1) (get8! (- ptr 3)))))

;; mark real ptr as free
(define-signature malloc/set-free! Pointer -> Void)
(define (malloc/set-free! ptr)
  (set8! (- ptr 3) (bior #b1 (get8! (- ptr 3)))))

;; update size of real ptr
(define-signature malloc/set-size! Pointer -> Number -> Void)
(define (malloc/set-size! ptr sz)
  (set! (- ptr 2) sz))

(define-signature malloc/create-block-after Pointer -> Number -> Void)
(define (malloc/create-block-after ptr sz)
  (let ((base (+ 3 ptr (malloc/get-block-size ptr))))
    (malloc/set-free! base)
    (malloc/set-size! base sz)))

(define-signature memset Pointer -> Number -> Number -> Void)
(define (memset b c len)
  (loopn (i 0 len 1)
    (set8! (+ b i) c)))

(define-signature malloc Number -> Pointer)
(define (malloc n)
  (let ((p (_malloc n (+ 3 *compiler-end*))))
    (memset p 0 n)
    p))

(define-signature _malloc Number -> Pointer -> Pointer)
(define (_malloc n ptr)
  (cond
   ((>= ptr (- #xffff malloc/margin))
    0) ; out of memory
   ((and (malloc/free? ptr) (>= (malloc/get-block-size ptr) n))
    (let ((rest-size (- (malloc/get-block-size ptr) n)))
      (if (> rest-size 4) ; we can insert another block later
          (begin
            (malloc/set-used! ptr)
            (malloc/set-size! ptr n)
            (malloc/create-block-after ptr (- rest-size 3))
            )
          (begin
            (malloc/set-used! ptr)))
      ptr))
   (else
    (_malloc n (+ ptr (malloc/get-block-size ptr) 3)))))

(define-signature malloc/next-block Pointer -> Pointer)
(define (malloc/next-block ptr)
  (+ ptr (malloc/get-block-size ptr) 3))

(define-signature malloc/connect-refs! Pointer -> Void)
(define (malloc/connect-refs! ptr)
  (if (and (<= ptr (- #xffff malloc/margin)) (>= ptr *compiler-end*))
      (let ((next (malloc/next-block ptr)))
        (if (and (malloc/free? ptr) (malloc/free? next))
            (begin
              (malloc/set-size! ptr (+ (malloc/get-block-size ptr) (malloc/get-block-size next) 3))
              (malloc/connect-refs! ptr))
            (malloc/connect-refs! (malloc/next-block ptr))))
      (noop)))

(define-signature free Pointer -> Void)
(define (free ptr)
  (malloc/set-free! ptr)
  (malloc/connect-refs! (+ 3 *compiler-end*)))

(define-signature realloc Pointer -> Number -> Pointer)
(define (realloc p size)
  (let* ((old (malloc/get-block-size p))
         (newptr (malloc size)))
    (memcpy newptr p old)
    (free p)
    newptr))

(define-macro-rule ()
  (delete x)
  (free x))
