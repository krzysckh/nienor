(define-sprite heart
  _ _ _ _ _ _ _ _
  _ _ X _ _ X _ _
  _ X X X X X X _
  _ X X X X X X _
  _ _ X X X X _ _
  _ _ _ X X _ _ _
  _ _ _ _ _ _ _ _
  _ _ _ _ _ _ _ _)

(define (main)
  (set-colors! #x0f00 #x0000 #x000)
  (set-draw-handler! draw))

(define-vector (draw)
  (fill! 0 0 0 0)
  (sprite! (mouse-x) (mouse-y) heart 0 0 0 0 1))
