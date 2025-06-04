(with-codegen-at! #x100
  ((main)
   (uxn-call! () brk) ; <- this will be later in a prelude

   ;; there are some non-standard parens in defun, but that's just for now
   ;; i'll create a better api later

   (defun push! (short)
     ((_push! _ short)))

   (defun pus! (byte)
     ((_push! byte byte)))

   (defun + (a b)
     ((push! a) ; i manually push here for the uxn-call! primitive
      (push! b)
      (uxn-call! (2) add)))

   (defun - (a b)
     ((push! a)
      (push! b)
      (uxn-call! (2) sub)))

   (defun putchar (c)
     ((pus! c)
      (pus! #x18)
      (uxn-call! () deo)))

   (defun exit (code)
     ((pus! code)
      (pus! #x0f)
      (uxn-call! () deo)))

   (defun noop ()
     ())

   (defun print-hello ()
     ((putchar #\h)
      (putchar #\e)
      (putchar #\l)
      (putchar #\l)
      (putchar #\o)
      (putchar #\newline)))

   (defun main ()
     ((if #t (print-hello) (noop))
      (exit (+ #x7f 1))))

   ))
