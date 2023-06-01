#lang r7rs

(define-library (ra device)
  (export device
          device?
          device->path
          device->input-port
          device->output-port)
  (import (scheme base)
          (libserialport))
  (begin
    (define-record-type <decive>
      (%device path input-port output-port)
      device?
      (path device->path)
      (input-port device->input-port)
      (output-port device->output-port))

    (define (device path)
      (let-values (((in out) (open-serial-port path)))
        (%device path in out)))))
