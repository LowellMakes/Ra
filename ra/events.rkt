#lang r7rs

(define-library (ra events)
  (include "events/exports.scm")
  (import (scheme base))
  (include "events/impl.scm"))
