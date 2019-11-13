(define (stream-limit s tol)
  (let ((a (stream-car s))
        (b (stream-cdar s)))
    (if (< (abs (- b a)) tol)
        b
        (stream-limit (stream-cdr s) tol))))
