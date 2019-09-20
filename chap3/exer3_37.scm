(define (c+ x y)
  (let ((z (make-connector)))
    (add x y z)
    z))

(define (c- z x)
  (let ((y (make-connector)))
    (add x y z)
    y))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ z x)
  (let ((y (make-connector)))
    (multiplier x y z)
    y))

(define (cv v)
  (let ((c (make-connector)))
    (constant v c)
    c))
