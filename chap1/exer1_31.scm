(define (product term a next b)
  (define (iter a result)
	(if (> a b)
		result
		(iter (next a) (* result (term a)))))
  (iter a 1))


(define (factorial n)
  (define (id x) x)
  (define (inc x) (+ x 1))
  (product id 1 inc n))


(define (calc_pi n)
  (define (term x) (/ (* x (+ x 2))
					  (square (+ x 1))))
  (define (next x) (+ x 2))
  (define right-hand (product term 2 next n))
  (* 4 right-hand))


(define (recursive-product term a next b)
  (if (> a b)
	  1
	  (* (term a)
		 (recursive-product term (next a) next b))))


(define (recursive-factorial n)
  (define (id x) x)
  (define (inc x) (+ x 1))
  (recursive-product id 1 inc n))
