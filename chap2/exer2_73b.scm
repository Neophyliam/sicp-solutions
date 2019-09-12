;; S-expression is used for representing expressions, so that the first entry
;; in the list is the tag.
(define (make-sum a1 a2) (list '+ a1 a2))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (make-product m1 m2) (list '* m1 m2))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (install-deriv-sum)
  (define (deriv-sum sum-exp var)
    (make-sum (deriv (addend sum-exp) var)
              (deriv (augend sum-exp) var)))
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-deriv-product)
  (define (deriv-product prod-exp var)
    (make-sum
      (make-product (multiplier prod-exp)
                    (deriv (multiplicand prod-exp) var))
      (make-product (deriv (multiplier prod-exp) var)
                    (multiplicand prod-exp))))
  (put 'deriv '* deriv-product)
  'done)
