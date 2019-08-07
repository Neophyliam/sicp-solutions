(define (project x) (apply-generic 'project x))
(define (droppable? x) (apply-generic 'droppable? x))

;; put in integer package
(put 'droppable? '(integer)
     (lambda (x) #f))

;; put in rational package
(put 'droppable? '(rational)
     (lambda (x) (= (denom x) 1)))
(put 'project '(rational)
     (lambda (x) (make-integer (numer x))))

;; put in real package (real means floating point)
(put 'droppable? '(real)
     (lambda (x) #t))
(put 'project '(real)
     (lambda (x)
       (let ((rat (rationalize (inexact->exact x) 1/100)))
         (make-rational (numerator rat) (denominator rat)))))

;; put in complex package
(put 'droppable? '(complex)
     (lambda (x) (= (imag-part x) 0)))
(put 'project '(complex)
     (lambda (x)
       (make-real (real-part x))))

(define (drop x)
  (if (droppable? x)
      (drop (project x))
      x))

;; apply-generic
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
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((same-level-args (raise-to-same-level a1 a2)))
                  (apply-generic op (car same-level-args)
                                 (cadr same-level-args))))
              (error "No method for these types"
                     (list op type-tags)))))))
