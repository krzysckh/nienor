(with-codegen-at! #x100
  ((main)

   ;; there are some non-standard parens in defun, but that's just for now
   ;; i'll create a better api later

   (defun + (a b)
     ((push! a)
      (push! b)
      (uxn-call! () add)))

   (defun putchar (c)
     ((push! c)
      (push! #x18)
      (uxn-call! () deo)))

   (defun exit (code)
     ((push! code)
      (push! #x0f)
      (uxn-call! () deo)))

   (defun main ()
     ((push! 69)
      (uxn-call! () pop)
      (putchar (+ #\f (+ 1 1)))
      (putchar #\e)
      (putchar #\l)
      (putchar #\l)
      (putchar #\o)
      (putchar #\newline)
      (exit (+ #x7f 1))))
   ))
