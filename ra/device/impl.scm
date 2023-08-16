(define-record-type <device>
  (%device path input-port output-port)
  device?
  (path device->path)
  (input-port device->input-port)
  (output-port device->output-port))

(define (device path)
  (let-values (((in out) (open-serial-port path)))
    (%device path in out)))

(define (available-devices)
  (map device (serial-ports)))
