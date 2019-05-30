(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(cond ((filter a) (iter (next a) (combiner result (term a))))
			  (else (iter (next a) (combiner result null-value))))))
  (iter a null-value))


(define (square-sum a b)
  (define (next x) (+ x 1))
  (filtered-accumulate + prime? 0 square a next b))


(define (product n)
  (define (prime-to-n? x)
	(= (gcd x n) 1))
  (define (next x) (+ x 1))
  (define (id x) x)
  (filtered-accumulate * prime-to-n? 1 id 1 next n))

;; prime?
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (square n) (* n n))

(define (next test-divisor) (if (= test-divisor 2) 3 (+ test-divisor 2)))

(define (prime? n)
  (= n (smallest-divisor n)))
