;;; Parameters
(define device (make-parameter #f))
(define profiles (make-parameter (list)))

;;; Internal utils
;; Assert that a device is connected
(define (assert-device-connected!)
  (unless (device)
    (error "No device connected!")))

;;; Commands
(define (reset)
  (assert-device-connected!)
  (parameterize ((current-output-port (device->output-port (device))))
    (write 'R)
    (newline)))

(define (start)
  (assert-device-connected!)
  (parameterize ((current-output-port (device->output-port (device))))
    (write 'S)
    (newline)))

(define (cool)
  (assert-device-connected!)
  (parameterize ((current-output-port (device->output-port (device))))
    (write 'C)
    (newline)))

(define (select-profile profile)
  (assert-device-connected!)
  (parameterize ((current-output-port (device->output-port (device))))
    (write (profile->code profile))
    (newline)))

(define (list-profiles)
  (assert-device-connected!)
  (parameterize ((current-output-port (device->output-port (device))))
    (write 'SP)
    (newline)))

(define (status)
  (assert-device-connected!)
  (parameterize ((current-output-port (device->output-port (device))))
    (write 'SS)
    (newline)))

(define (header)
  (assert-device-connected!)
  (parameterize ((current-output-port (device->output-port (device))))
    (write 'SH)
    (newline)))

;;; Parsing
(define (parse-line line)
  (define (oven-state? line )
    (and (> (string-length line) 2)
         (string=? (substring line 0 2) "D,")))
  (define (line->oven-state line)
    (oven-state (string->symbol (substring line 2))))

  (define (profile? line)
    (and (> (string-length line) 2)
	 (char=? #\P (string-ref line 0))
	 (member (string-ref line 1) (list #\0 #\1 #\2 #\3))
	 (char=? #\, (string-ref line 2))))
  (define (line->profile line)
    (call-with-port (open-input-string line)
      (lambda (p)
	(let* ((code (read p))
	       (_ (read-char p))
	       (name (read-line p)))
	  (profile name code)))))

  (define (cool? line)
    (and (> (string-length line) 2)
         (string=? (substring line 0 2) "C,")))
  (define (line->cool line)
    (e:cool (string=? (substring line (string-length "C,COOL ")) "ON")))

  (define (temperature? line)
    (and (> (string-length line) 2)
         (string=? (substring line 0 2) "T,")))
  (define (line->temperature line)
    (call-with-port (open-input-string line)
      (lambda (p)
	(read p) ; throw away the first T
	(let* ((seconds (second (read p)))
	       (side (second (read p)))
	       (front (second (read p)))
	       (goal (second (read p))))
	  (temperature seconds side front goal)))))

  ;; Remove the trailing #\return if it exists
  (let* ((len (string-length line))
	 (line (if (and (> len 1)
			(eq? (string-ref line (- len 1)) #\return))
		   (substring line 0 (- (string-length line) 1))
		   line)))
    (cond
     ((oven-state? line) (line->oven-state line))
     ((profile? line) (line->profile line))
     ((cool? line) (line->cool line))
     ((temperature? line) (line->temperature line))
     (else line))))

;; Return the stream of events from the input port
(define-stream (stream-events port)
  (stream-cons (parse-line (read-line port))
	       (stream-events port)))

;;; Event Loop
;; Event loop to dispatch input events to each event handler
(define (event-loop on-oven-state
                    on-profile
                    on-cool
                    on-temperature
                    on-plain-message)
  (assert-device-connected!)
  (stream-for-each
   (lambda (msg)
     (cond
      ((oven-state? msg) (on-oven-state msg))
      ((profile? msg) (on-profile msg))
      ((cool? msg) (on-cool msg))
      ((temperature? msg) (on-temperature msg))
      ((string? msg) (on-plain-message msg))
      (else (error "Unhandled event" msg))))
   (stream-events (device->input-port (device)))))

;; DSL to build the event loop.
;; Pretty gnarly implementation, but the API is nice.
(define-syntax handle-events
  (syntax-rules (:on-oven-state
                 :on-profile
                 :on-cool
                 :on-temperature
                 :on-plain-message)
    ((_ (:on-oven-state on-oven-state
         :on-profile on-profile
         :on-cool on-cool
         :on-temperature on-temperature
         :on-plain-message on-plain-message))
     (event-loop on-oven-state
                 on-profile
                 on-cool
                 on-temperature
                 on-plain-message))
    ((_ (:on-oven-state _
         :on-profile on-profile
         :on-cool on-cool
         :on-temperature on-temperature
         :on-plain-message on-plain-message) :on-oven-state on-oven-state args ...)
     (handle-events (:on-oven-state on-oven-state
                     :on-profile on-profile
                     :on-cool on-cool
                     :on-temperature on-temperature
                     :on-plain-message on-plain-message) args ...))
    ((_ (:on-oven-state on-oven-state
         :on-profile _
         :on-cool on-cool
         :on-temperature on-temperature
         :on-plain-message on-plain-message) :on-profile on-profile args ...)
     (handle-events (:on-oven-state on-oven-state
                     :on-profile on-profile
                     :on-cool on-cool
                     :on-temperature on-temperature
                     :on-plain-message on-plain-message) args ...))
    ((_ (:on-oven-state on-oven-state
         :on-profile on-profile
         :on-cool _
         :on-temperature on-temperature
         :on-plain-message on-plain-message) :on-cool on-cool args ...)
     (handle-events (:on-oven-state on-oven-state
                     :on-profile on-profile
                     :on-cool on-cool
                     :on-temperature on-temperature
                     :on-plain-message on-plain-message) args ...))
    ((_ (:on-oven-state on-oven-state
         :on-profile on-profile
         :on-cool on-cool
         :on-temperature _
         :on-plain-message on-plain-message) :on-temperature on-temperature args ...)
     (handle-events (:on-oven-state on-oven-state
                     :on-profile on-profile
                     :on-cool on-cool
                     :on-temperature on-temperature
                     :on-plain-message on-plain-message) args ...))
    ((_ (:on-oven-state on-oven-state
         :on-profile on-profile
         :on-cool on-cool
         :on-temperature on-temperature
         :on-plain-message _) :on-plain-message on-plain-message args ...)
     (handle-events (:on-oven-state on-oven-state
                     :on-profile on-profile
                     :on-cool on-cool
                     :on-temperature on-temperature
                     :on-plain-message on-plain-message) args ...))
    ((_ args ...)
     (handle-events (:on-oven-state (lambda _ (values))
                     :on-profile (lambda _ (values))
                     :on-cool (lambda _ (values))
                     :on-temperature (lambda _ (values))
                     :on-plain-message (lambda _ (values))) args ...))))
