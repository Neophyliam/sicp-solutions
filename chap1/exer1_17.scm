(define (double i)
  (* 2 i))

(define (halve i)
  (/ i 2))

(define (mult a b)
  (cond ((= b 0) 0)
	((even? b) (mult (double a) (halve b)))
	(else (+ a (mult a (- b 1))))))
