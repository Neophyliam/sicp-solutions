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

;; prime?
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

(define (square n) (* n n))

(define (next test-divisor) (if (= test-divisor 2) 3 (+ test-divisor 2)))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (unique-pairs n)
  (accumulate append (list)
              (map (lambda (i)
                     (map (lambda (j)
                            (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;; test
(display (unique-pairs 5))
(newline)
(display (prime-sum-pairs 6))
(newline)
