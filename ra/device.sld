(define-library (ra device)
  (include-library-declarations "device/exports.scm")
  (import (scheme base))
  (cond-expand
   (else (error "Missing import for serial port library")))
  (include "device/impl.scm"))
