(define number-6 1000037)
(define number-10 10000000019)
(define number-11 100000000019)
(define number-12 1000000000039)

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
  (define (try-it a)
	(= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else #f)))

(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (fast-prime? n 100)
  	  (report-prime (- (tms:clock (times)) start-time))))
  
  (define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

  (newline)
  (display n)
  (start-prime-test n (tms:clock (times))))

(define (search-for-primes start end)
  (cond ((> start end) (newline))
	((even? start) (search-for-primes (+ start 1) end))
	;; Do not enclose the two procedure applications in a pair of parenthesis.
	(else (timed-prime-test start)
		  (search-for-primes (+ start 2) end))))

(define sfp search-for-primes)
