#lang r7rs

(define-library (ra device)
  (include "device/exports.scm")
  (import (except (scheme base) map)
          (only (srfi 1) map))
  (cond-expand
   (racket (import (libserialport)))
   (else (error "Missing import for serial port library")))
  (include "device/impl.scm"))
