#lang r7rs

(define-library (ra)
  (include "exports.scm")
  (import (scheme base)
          (only (racket base) read)
          (scheme write)
          (ra device)
          (ra events)
          (prefix (ra events) e:)
          (only (srfi 1) second)
          (srfi 41))
  (include "impl.scm"))
