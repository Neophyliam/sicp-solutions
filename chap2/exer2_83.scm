(define (raise-integer x)
  (make-rational (contents x) 1))

(define (raise-rational x)
  (let ((rational (contents x)))
    (make-real (/ (numer rational) (denom rational)))))

(define (raise-real x)
  (let ((real (contents x)))
    (make-complex-from-real-imag real 0)))

;; generic
(define (raise x) (apply-generic 'raise x))

;; insert in integer package
(put 'raise '(integer) raise-integer)

;; insert in rational package
(put 'raise '(rational) raise-rational)

;; insert in real package
(put 'raise '(real) raise-real)
