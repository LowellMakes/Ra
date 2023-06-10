#lang r7rs

(import (scheme read)
        (scheme write)
        (only (srfi 1) iota for-each delete remove)
        (srfi 28) ; format
        (except (racket base) remove)
        (libserialport)
        (rename (ra)
                (device current-device))
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

(define (same-profile-code? p1 p2 . ps)
  (if (eq? (profile->code p1)
           (profile->code p2))
      (or (null? ps)
          (apply same-profile-code? p1 ps))
      #f))

(thread
 (lambda ()
   (handle-events
    ;; :on-oven-state  (lambda (msg) (write msg) (newline))
    :on-profile (lambda (msg) (profiles (cons msg (remove (lambda (p) (same-profile-code? p msg)) (profiles)))))
    ;; :on-cool        (lambda (msg) (write msg) (newline))
    ;; :on-temperature (lambda (msg) (write msg) (newline))
    :on-plain-message (lambda (msg) (display msg) (newline))
    )))
