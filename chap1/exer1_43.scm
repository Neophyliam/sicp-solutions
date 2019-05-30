(define (repeated f n)
  (define (iter i result)
	(if (= i 0)
		result
		(iter (- i 1) (f result))))

  (lambda (x) (iter n x)))


;; test
((repeated square 2) 5)
