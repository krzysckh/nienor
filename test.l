(with-codegen-at! #x0100
  ((define-label! plus)
   (uxn-call! () add)
   (uxn-call! (2 r) jmp)))
