#lang r7rs

(define-library (ra)
  ;; Types
  (export profile
          profile?
          profile->name
          profile->code)
  ;; Parameters
  (export device
          profiles)
  ;; Commands
  (export reset
          start
          cool
          select-profile
          current-profile
          status
          header)
  (export parse-input)
  (import (scheme base)
          (scheme read)
          (scheme write)
          (ra device))
  ;; Types
  (begin
    (define-record-type <profile>
      (profile name code)
      profile?
      ;; Human readable name
      (name profile->name)
      ;; Code associated with it on the Arduino
      (code profile->code)))
  ;; Parameters
  (begin
    (define device (make-parameter #f))
    (define profiles (make-parameter (list))))
  ;; Commands
  (begin
    ;; Assert that a device is connected
    (define (device-connected!)
      (unless (device)
        (error "No device connected!")))
    
    (define (reset)
      (device-connected!)
      (display "R\n" (device->output-port (device))))

    (define (start)
      (device-connected!)
      (display "S\n" (device->output-port (device))))

    (define (cool)
      (device-connected!)
      (display "C\n" (device->output-port (device))))

    (define (select-profile profile)
      (device-connected!)
      (display (string-append (profile->code profile) "\n")
               (device->output-port (device))))

    (define (current-profile)
      (device-connected!)
      (display "SP\n" (device->output-port (device))))

    (define (status)
      (device-connected!)
      (display "SS\n" (device->output-port (device))))

    (define (header)
      (device-connected!)
      (display "SH\n" (device->output-port (device)))))
  (begin
    (define (parse-input on-reset
                         on-phase-change
                         on-selected-profile
                         on-status
                         on-header
                         on-cool
                         on-profile
                         on-plain-message)
      (device-connected!)
      (define (reset? line)
        (string=? line "OVEN RESET"))

      (define (phase-change? line)
        (and (> (string-length line) 2)
             (string=? (substring line 0 2) "D,")))

      (define (profile? line)
        (and (> (string-length line) 2)
             (string=? (substring line 0 2) "P,")))

      (define (status? line)
        (string=? line "Oven State Begin"))

      (define (header? line)
        (let ((header-start "Send Oven a Serial ASCII Command to execute"))
          (string=? (substring line 0 (string-length header-start))
                    header-start)))

      (define (cool? line)
        (and (> (string-length line) 2)
             (string=? (substring line 0 2) "C,")))

      (parameterize ((current-input-port (device->input-port (device))))
        (let loop ((current-parser on-header)
                   (line (read-line)))
          (cond
            ((reset? line) (loop on-header (read-line)))
            ((phase-change? line)
             
             (loop current-parser (read-line)))))))))
