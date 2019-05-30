(define (sum term a next b)
		(if (> a b)
			0
			(+ (term a)
			   (sum term (next a) next b))))

(define (integral f a b n)
	(define h (/ (- b a) n))

	(define (term x)
			(+ (f x)
			   (* 4
				  (f (+ x h)))
			   (f (+ x (* h 2)))))

	(define (next x)
			(+ x (* 2 h)))
  
		(* (sum term a next (- b (* 2 h)))
		   (/ h 3)))

(define (cube x) (* x x x))
;; Test
;; (integral cube 0 1. 22)  ;; will have a huge error
;; Maybe it's because of round-off error.
;; (integral cube 0 1 22)  ;; will be equal to 1/4
;; The solution in exer1_29_v2.scm is much more stable.
