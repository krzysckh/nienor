(_declare-test
 output => "Hello World!\n\neot\n")

(define-macro-rule (_)
  (bf . code)
  (_bf (_) (_) code))

(define-macro-rule (())
  (_bf l r ())
  (puts "\neot\n"))

;; +
(define-macro-rule (+ _ I)
  (_bf (_ . left) (_ (_ . v) . right)   (+ . code))
  (_bf (_ . left) (_ (_ I . v) . right) code))

(define-macro-rule (+ _ I)
  (_bf (_ . left) (_)       (+ . code))
  (_bf (_ . left) (_ (_ I)) code))

(define-macro-rule (+ _ I)
  (_bf (_ . left)
       (_ (_ I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I ; overflow
           I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
           I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
           I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
           I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
           I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
           I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
           I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I)
          . right)
       (+ . code))
  (_bf (_ . left) (_ (_) . right) code))
;; end +

;; -
(define-macro-rule (- _)
  (_bf (_ . left) (_ (_ v . rest) . right) (- . code))
  (_bf (_ . left) (_ (_ . rest) . right)   code))

(define-macro-rule (- _)
  (_bf (_ . left) (_)     (- . code))
  (_bf (_ . left) (_ (_)) (- . code)))

(define-macro-rule (- _)
  (_bf (_ . left) (_ (_) . right) (- . code))
  (_bf (_ . left) (_ (_ I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I ; underflow
                      I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
                      I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
                      I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
                      I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
                      I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
                      I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I
                      I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I) . right) code))
;; end -

;; >
(define-macro-rule (> _)
  (_bf (_ . left)   (_ v . right) (> . code))
  (_bf (_ v . left) (_ . right)   code))

(define-macro-rule (> _)
  (_bf (_ . left)     (_)  (> . code))
  (_bf (_ (_) . left) (_) code))
;; end >

;; <
(define-macro-rule (< _)
  (_bf (_ v . left) (_ . right)   (< . code))
  (_bf (_ . left)   (_ v . right) code))

(define-macro-rule (< _)
  (_bf (_) (_ . right) (< . code))
  (compiler-error "underflow"))
;; end <

;; . -- renamed to p, as . is syntax in scheme
(define-macro-rule ()
  (_cnt _ . vs)
  (+ 1 (_cnt . vs)))

(define-macro-rule ()
  (_cnt)
  0)

(define-macro-rule (p _)
  (_bf (_ . left) (_ (_ . c) . right) (p . code))
  (begin
    (putchar (_cnt . c))
    (_bf (_ . left) (_ (_ . c) . right) code)))

(define-macro-rule (p _)
  (_bf (_ . left) (_)   (p . code))
  (_bf (_ . left) (_ (_)) (p . code)))
;; end .

(define-macro-rule (_)
  (flapp . vs)
  (flapp (_) . vs))

(define-macro-rule (_)
  (flapp (_ . rs) v . vs)
  (flapp (_ v . rs) . vs))

(define-macro-rule (_)
  (flapp (_ . rs) (a . as) . vs)
  (flapp (_ a . rs) as . vs))

(define-macro-rule (_ ())
  (flapp (_ . rs) () . vs)
  (flapp (_ . rs) . vs))

(define-macro-rule (_)
  (flapp (_ . rs))
  (__m_reverse . rs))

;; [
(define-macro-rule (_)
  (_bf (_ . left) (_)     ((x . xs) . code))
  (_bf (_ . left) (_ (_)) ((x . xs) . code)))

(define-macro-rule (_)
  (_bf (_ . left) (_ v . right) ((x . xs) . code))
  (_bf (_ . left) (_ v . right) (flapp
                                 (x . xs)
                                 ((x . xs))
                                 code)))

(define-macro-rule (_)
  (_bf (_ . left) (_ (_) . right) ((x . xs) . code))
  (_bf (_ . left) (_ (_) . right) code))
;; end [

(define (main)
  (bf
   + + + + + + + + + +
   ( > + + + + + + + > + + + + + + + + + + > + + + > + < < < < - )
   > + + p
   > + p
   + + + + + + + p
   p
   + + + p
   > + + p
   < < + + + + + + + + + + + + + + + p
   > p
   + + + p
   - - - - - - p
   - - - - - - - - p
   > + p
   > p
   )
  (exit!))
