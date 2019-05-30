(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
	(< (abs (- v1 v2))
	   tolerance))
  (define (try guess)
	(let ((next (f guess)))
	  (if (close-enough? guess next)
		  next
		  (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (repeated f n)
  (define (iter i result)
	(if (= i 0)
		result
		(iter (- i 1) (f result))))

  (lambda (x) (iter n x)))

(define (power x n)
  (if (= n 0) 1 (* x (power x (- n 1)))))

;; test
(define (fourth-root x)
  (fixed-point ((repeated average-damp 2) 
				(lambda (y) (/ x (* y y y))))
			   1.0))

(define (fifth-root x)
  (fixed-point ((repeated average-damp 2) 
				(lambda (y) (/ x (* y y y y))))
			   1.0))

(define (sixth-root x)
  (fixed-point ((repeated average-damp 2) 
				(lambda (y) (/ x (power y 5))))
			   1.0))

(define (seventh-root x)
  (fixed-point ((repeated average-damp 2) 
				(lambda (y) (/ x (power y 6))))
			   1.0))

(define (eighth-root x)
  (fixed-point ((repeated average-damp 3) 
				(lambda (y) (/ x (power y 7))))
			   1.0))

(define (11-root x)
  (fixed-point ((repeated average-damp 3) 
				(lambda (y) (/ x (power y 10))))
			   1.0))

(define (12-root x)
  (fixed-point ((repeated average-damp 3) 
				(lambda (y) (/ x (power y 11))))
			   1.0))

(define (15-root x)
  (fixed-point ((repeated average-damp 3) 
				(lambda (y) (/ x (power y 14))))
			   1.0))

(define (16-root x)
  (fixed-point ((repeated average-damp 4) 
				(lambda (y) (/ x (power y 15))))
			   1.0))
;; end test

(define (log2 x) (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (log2 n))) 
				(lambda (y) (/ x (power y (- n 1)))))
			   1.0))
