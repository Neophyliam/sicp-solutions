(define (square-list items)
  (if (null? items)
      (list)
      (cons (expt (car items) 2)
            (square-list (cdr items)))))

(define (square x) (* x x))

(define (square-list2 items)
  (map square items))
