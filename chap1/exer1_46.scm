(define (iterative-improve good-enough? improve)
  (define (try guess)
	(let ((next (improve guess)))
	  (if (good-enough? guess next)
		  next
		  (try next))))
  try)

;; test
(define tolerance 0.00001)

(define (good-enough? a b)
  (< (abs (- b a)) tolerance))

(define (fixed-point f first-guess)
  ((iterative-improve good-enough? f)
   first-guess))

(define phi
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(define (sqrt x)
  (define (improve guess)
	(/ (+ guess (/ x guess)) 2))
  ((iterative-improve good-enough? improve) 1.0))
