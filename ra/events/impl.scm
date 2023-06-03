(define-record-type <oven-state>
  (oven-state state)
  oven-state?
  (state oven-state->state))

(define-record-type <profile>
  (profile name code)
  profile?
  ;; Human readable name
  (name profile->name)
  ;; Code associated with it on the Arduino
  (code profile->code))

(define-record-type <cool>
  (cool on)
  cool?
  (on cool-on?))

(define-record-type <temperature>
  (temperature seconds side front goal)
  temperature?
  (seconds temperature->seconds)
  (side temperature->side)
  (front temperature->front)
  (goal temperature->goal))
