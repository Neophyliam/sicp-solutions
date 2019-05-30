(define (cont-frac n d k)
  (define (iter i result)
	(if (= i 0)
		result
		(iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(define (n i) 1.)
(define (d i)
  (let ((seq (remainder i 3))
		(quot (quotient i 3)))
	(cond ((or (= seq 1) (= seq 0)) 1)
		  (else (* (+ quot 1) 2)))))

(define (calc-e k)
  (+ 2 (cont-frac n d k)))
