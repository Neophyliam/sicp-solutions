(define (cons x y)
  (* (expt 2 x) (expt 3 y)))
(define (car z)
  (define (iter x z)
    (if (not (= (remainder z 2) 0))
        x
        (iter (+ x 1) (quotient z 2))))
  (iter 0 z))
(define (cdr z)
  (define (iter x z)
    (if (not (= (remainder z 3) 0))
        x
        (iter (+ x 1) (quotient z 3))))
  (iter 0 z))
