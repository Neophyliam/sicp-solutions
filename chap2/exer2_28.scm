(define (fringe li)
  (define (iter left result)
    (cond ((null? left) result)
          ((pair? (car left))
           (iter (cdr left) (iter (car left) result)))
          (else
            (iter (cdr left) (cons (car left) result)))))
  (reverse (iter li (list))))
