(define (split large-proc small-proc)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split large-proc small-proc)
                        painter
                        (- n 1))))
          (large-proc painter (small-proc smaller smaller))))))
