(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b n a)
  (cond ((= n 1) (* b a))
	((even? n) (expt-iter (* b b) (/ n 2) a))
	(else (expt-iter b (- n 1) (* a b)))))

(define (even? n)
  (= (remainder n 2) 0))
