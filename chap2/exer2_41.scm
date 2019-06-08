(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
          (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (sum-triple triple)
  (accumulate + 0 triple))

(define (make-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triple-sum-equal-to n s)
  (filter (lambda (triple) (= (sum-triple triple) s))
          (make-triples n)))

;; test
(display (make-triples 5))
(newline)
(display (triple-sum-equal-to 5 8))
(newline)
