(define (div-series num denom)
  (let ((constant (stream-car denom)))
    (if (= constant 0)
        (error "zero constant term in denominator -- DIV-SERIES")
        (let ((factor (/ 1 constant)))
          (let ((new-denom (scale-stream denom factor))
                (new-num (scale-stream num factor)))
            (mul-series new-num (reciprocal new-denom)))))))

(define tangent-series
  (div-series sine-series cosine-series))
