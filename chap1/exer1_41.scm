(define (double f)
  (lambda (x) (f (f x))))


;; test
(define (inc x) (+ x 1))
(define add2 (double inc))

(((double (double double)) inc) 5)
