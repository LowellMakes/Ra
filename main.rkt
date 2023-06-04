#lang r7rs

(import (scheme read)
        (scheme write)
        (only (srfi 1) iota for-each delete)
        (srfi 28) ; format
        (racket base)
        (libserialport)
        (rename (ra)
                (device current-device)
                (select-profile ra:select-profile))
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

(define profiles (list))
(define last-selected-profile #f)

(define (select-profile profile)
  (set! last-selected-profile profile)
  (ra:select-profile profile))

(define continue-event-loop #f)

(define get-next-profile #f)
(define (get-all-profiles)
  (call/cc
   (lambda (leave)
     (for-each
      (lambda (profile-code)
        (call/cc
         (lambda (next)
           (set! get-next-profile next)
           (select-profile (profile "" profile-code))
           (if continue-event-loop
               (continue-event-loop)
               (leave))))
        (set! get-next-profile #f))
      '(P0 P1 P2 P3)))))

(thread
 (lambda ()
   (handle-events
    ;; :on-oven-state  (lambda (msg) (write msg) (newline))
    :on-profile (lambda (msg)
                  (when last-selected-profile
                    (set! profiles
                          (sort (cons (profile (profile->name msg) (profile->code last-selected-profile))
                                      (delete msg
                                              profiles
                                              (lambda (p1 p2)
                                                (string=? (profile->name p1)
                                                          (profile->name p2)))))
                                (lambda (p1 p2)
                                  (string<? (symbol->string (profile->code p1)) (symbol->string (profile->code p2)))))))
                  (when get-next-profile
                    (call/cc
                     (lambda (continue)
                       (set! continue-event-loop continue)
                       (get-next-profile)))
                    (set! continue-event-loop #f)))
    ;; :on-cool        (lambda (msg) (write msg) (newline))
    ;; :on-temperature (lambda (msg) (write msg) (newline))
    :on-plain-message (lambda (msg) (display msg) (newline))
    )))
