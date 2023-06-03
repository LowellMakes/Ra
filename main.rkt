#lang r7rs

(import (scheme read)
        (scheme write)
        (only (srfi 1) iota for-each)
        (srfi 28) ; format
        (racket base)
        (libserialport)
        (rename (ra) (device current-device))
        (ra device))

(define (select-device)
  (let ((serial-ports (serial-ports)))
    (for-each
     (lambda (index path)
       (display (format "~a: ~a~%" index path)))
     (iota (length serial-ports))
     serial-ports)
    (display "> ")
    (current-device (device (list-ref serial-ports (read))))))

(thread
 (lambda ()
   (handle-events
    :on-oven-state  (lambda (msg) (write msg) (newline))
    :on-profile     (lambda (msg) (write msg) (newline))
    :on-cool        (lambda (msg) (write msg) (newline))
    :on-temperature (lambda (msg) (write msg) (newline))
    ;; :on-plain-message (lambda (msg) (display msg) (newline))
    )))
