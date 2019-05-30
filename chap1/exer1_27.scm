(define (square n)
  (* n n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder
	   (square (expmod base (/ exp 2) m))
	   m))
	(else
	  (remainder
		(* base (expmod base (- exp 1) m))
		m))))

(define (fermat-test n)
  (define (test-loop a)
	(cond ((= a n) #t)
		  ((not (= (expmod a n n) a)) #f)
		  (else (test-loop (+ a 1)))))
  (test-loop 1))
