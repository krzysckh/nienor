(_declare-test
 output => "px: 42\npy: 255\nalive?: 1\ndead?: 0\ntest: 21\n_pad: 37\nshort1: 65278\nshort2: 49406\nshort3: 47802\n")

(define-struct player
  short1
  (px 8)
  (py 8)
  short2
  (alive? 1)
  (dead? 1)
  (test 5)
  (_pad 9)
  short3)

(define (main)
  (with (make-player) as p
    (set-player-short1! p #xfefe)
    (set-player-px! p 42)
    (set-player-py! p 255)
    (set-player-short2! p #xc0fe)
    (set-player-short3! p #xbaba)
    (set-player-test! p #b10101)
    (set-player-alive?! p 1)
    (set-player-dead?! p 0)
    (set-player-_pad! p 37)
    (puts "px: ") (print-number (player-px p))
    (puts "py: ") (print-number (player-py p))
    (puts "alive?: ") (print-number (player-alive? p))
    (puts "dead?: ") (print-number (player-dead? p))
    (puts "test: ") (print-number (player-test p))
    (puts "_pad: ") (print-number (player-_pad p))
    (puts "short1: ") (print-number (player-short1 p))
    (puts "short2: ") (print-number (player-short2 p))
    (puts "short3: ") (print-number (player-short3 p))
    )
  (exit!))
