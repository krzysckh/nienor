;; Memory layout after *compiler-end*:
;;
;; 0x.. 0x.. [data] 0x.. 0x.. [data]
;; \__HDR__/        \__HDR__/
;;   ↑                   ↑
;; header with the size of allocated block
;; if the lowest bit is set, the block is free, else it's used
;; it's then assumed that the low bit is always one, so a block might get allocated
;; 1 byte more than requested

(define (main)
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
