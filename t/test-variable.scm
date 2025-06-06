;; TODO: since defvar is a wrapper for _alloc!,
;; consider doing something to be able to make it more...
;; DWIM, so (defvar _ 42) => (defvar _ 0 42)
;; and leave alloc! for do as i say stuff

;; Also consider making variables 1st class citizens, known about
;; by the compiler, so one could potentially define it with a name
;; and later implicitly insert get! into places where it knows a variable
;; and not a label nor a constant would be

(defvar *variable-a* 8 89)
(defvar *variable-b* 0 21)
(defvar *variable-c* 0 42)

(define (main)
  (print-number (get! *variable-a*))
  (print-number (get! *variable-b*))
  (print-number (get! *variable-c*))

  (debug!)
  (exit 128))
