(define (double i)
  (* 2 i))

(define (halve i)
  (/ i 2))

(define (mult a b)
  (mult-iter a b 0))

(define (mult-iter a b c)
  (cond ((= b 1) (+ a c))
	((even? b) (mult-iter (double a) (halve b) c))
	(else (mult-iter a (- b 1) (+ a c)))))
