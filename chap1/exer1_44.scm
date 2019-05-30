(define dx 0.00001)

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
					(f x)
					(f (+ x dx)))
				 3)))

(define (repeated f n)
  (define (iter i result)
	(if (= i 0)
		result
		(iter (- i 1) (f result))))

  (lambda (x) (iter n x)))

(define (nfold-smooth f n)
  (lambda (x)
	(((repeated smooth n) f) x)))


;; test
(define smoothed-square (smooth square))
(define smoothed-square-2 (nfold-smooth square 1))
(define smoothed2-square (nfold-smooth square 2))
