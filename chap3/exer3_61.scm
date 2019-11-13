(define (reciprocal s)
  (let ((sr (stream-cdr s)))
    (define x (cons-stream
                1
                (mul-series (scale-stream sr -1) x)))
    x))
