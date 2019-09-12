(define (sub x y) (apply-generic 'sub x y))
(define (negative x) (apply-generic 'negative x))

;; put in polynomial package
(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list (negative p2))))
      (error "Polys not insame var -- SUB-POLY"
             (list p1 p2))))

(define (negative p)
  (make-poly (variable p)
             (map (lambda (term)
                    (make-term (order term) (negative (coeff term))))
                  (term-list p))))

(put 'sub '(polynomial polynomial)
     (lambda (x y) (tag (sub-poly x y))))
(put 'negative '(polynomial)
     (lambda (x) (tag (negative x))))

;; These two generic operations should be installed in other packages too.
