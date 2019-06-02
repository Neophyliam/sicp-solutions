(define (last-pair li)
  (cond ((null? li) (list))
        ((null? (cdr li)) li)
        (else (last-pair (cdr li)))))
