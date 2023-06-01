#lang r7rs

(import (except (scheme base) for-each length list-ref)
        (scheme read)
        (scheme write)
        (only (srfi 1) iota for-each)
        (srfi 28) ; format
        (only (racket base) length list-ref thread)
        (libserialport)
        (rename (ra) (input-port ra:input-port) (output-port ra:output-port))
        (ra device))

;; See: https://github.com/lexi-lambda/racket-r7rs/issues/3#issuecomment-199717804
(import (only (rename (racket base) (list ilist)) ilist)
        (only (compatibility mlist) mlist->list))

(define current-device (make-parameter #f))

(define (select-device)
  (let ((serial-ports (serial-ports)))
    (for-each
     (lambda (index path)
       (display (format "~a: ~a~%" index path)))
     (iota (length serial-ports))
     serial-ports)
    (display "> ")
    (current-device (device (list-ref serial-ports (read))))
    (ra:input-port (device->input-port (current-device)))
    (ra:output-port (device->output-port (current-device)))))

(thread
 (lambda ()
   (parameterize ((current-input-port (device->input-port (current-device))))
     (let loop ((line (read-line)))
       (display line)
       (newline)
       (loop (read-line))))))

(profiles (mlist->list (list (profile "Profile A" "P0")
                             (profile "Profile B" "P1")
                             (profile "Profile C" "P2")
                             (profile "Profile D" "P3"))))
