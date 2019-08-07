(define (type-level x) (apply-generic 'type-level x))

;; insert in integer package
(put 'type-level '(integer) (lambda (x) 1))
;; insert in rational package
(put 'type-level '(rational) (lambda (x) 2))
;; insert in real package
(put 'type-level '(real) (lambda (x) 3))
;; insert in complex package
(put 'type-level '(complex) (lambda (x) 4))

(define (higher? x y)
  (let ((x-level (type-level x))
        (y-level (type-level y)))
    (> x-level y-level)))

(define (raise-to-same-level x y)
  (cond ((higher? x y)
         (raise-to-same-level x (raise y)))
        ((higher? y x)
         (raise-to-same-level (raise x) y))
        (else
          (list x y))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((same-level-args (raise-to-same-level a1 a2)))
                  (apply-generic op (car same-level-args)
                                 (cadr same-level-args))))
              (error "No method for these types"
                     (list op type-tags)))))))
