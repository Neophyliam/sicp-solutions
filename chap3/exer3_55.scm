(define (partial-sums s)
  (define result
    (cons-stream (stream-car s)
                 (add-streams result (stream-cdr s))))
  result)
