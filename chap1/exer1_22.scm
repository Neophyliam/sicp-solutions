(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (square n) (* n n))

(define (prime? n)
  (= n (smallest-divisor n)))

(define test-number 100000000019)   ;; test number: 100000000019
(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (prime? n)
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
