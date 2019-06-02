(define (same-parity ref . others)
  (define (iter ref left result)
    (cond ((null? left) result)
          ((= (remainder ref 2)
              (remainder (car left) 2))
           (iter ref (cdr left) (cons (car left) result)))
          (else
            (iter ref (cdr left) result))))
  (reverse (iter ref others (list))))
