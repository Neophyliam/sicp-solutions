(define (sum term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a) (+ result (term a)))))
  (iter a 0))


;; test
(define (term x) x)
(define (inc x) (+ x 1))
(sum term 1 inc 1000000)
