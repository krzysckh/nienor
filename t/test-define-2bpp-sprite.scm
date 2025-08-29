;; Well you can't use any symbol that is a macro so no -+/*
(define-2bpp-sprite heart (_ % & @)
  _ _ % _ _ % _ _
  _ % & % % & % _
  % & & & & & & %
  % & & @ @ & & %
  _ % & @ @ & % _
  _ _ % & & % _ _
  _ _ _ % % _ _ _
  _ _ _ _ _ _ _ _)

;; the same as the above guy
(define-2bpp-sprite heart2 (a b c d)
  a a b a a b a a
  a b c b b c b a
  b c c c c c c b
  b c c d d c c b
  a b c d d c b a
  a a b c c b a a
  a a a b b a a a
  a a a a a a a a)

(define (main)
  (set-colors! #x0f00 #x00f0 #x000f)
  (set-draw-handler! draw))

(define-vector (draw)
  (fill! 0 0 0 0)
  (sprite! (mouse-x) (mouse-y) heart 1 0 0 0 1)
  (sprite! (+ (mouse-x) 8) (mouse-y) heart2 1 0 0 0 1))
