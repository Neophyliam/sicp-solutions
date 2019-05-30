(define (recursive-fn n)
  (cond ((< n 3) n)
	(else (+ (recursive-fn (- n 1))
			 (* (recursive-fn (- n 2)) 2)
			 (* (recursive-fn (- n 3)) 3)))))

(define (fn n)
  (fn-iter 2 1 0 n))

(define (fn-iter a b c n)
  (cond ((= n 0) c)
	(else (fn-iter (+ a (* 2 b) (* 3 c)) a b (- n 1)))))
