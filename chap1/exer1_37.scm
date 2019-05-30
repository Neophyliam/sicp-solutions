(define (cont-frac n d k)
  (define (iter i result)
	(if (= i 0)
		result
		(iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))


;; test
(define (func i) 1.0)
(cont-frac func func 1)


(define (recursive-cont-frac n d k)
  (define (term i)
	(if (= i k)
		(/ (n k) (d k))
		(/ (n i) (+ (d i)
					(term (+ i 1))))))
  (term 1))
