(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2))
	   tolerance))
  (define (try guess)
	(newline)
	(display guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		  next
		  (try next))))
  (try first-guess))


(define (average a b) (/ (+ a b) 2))
(define (no-ad x) (/ (log 1000) (log x)))
(define (ad x) (average x
						(/ (log 1000) (log x))))

(fixed-point no-ad 1.5)

(fixed-point ad 1.5)
