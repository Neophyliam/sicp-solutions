(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                  ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))


;; test
(define (println x)
  (display x)
  (newline))

(println (union-set '() '()))
(println (union-set '() '(1)))
(println (union-set '(1) '()))
(println (union-set '(1) '(1)))
(println (union-set '(0) '(1)))
(println (union-set '(1) '(0)))
(println (union-set '(1 2) '(1)))
(println (union-set '(1 2) '(1 2)))
(println (union-set '(1 3 5 7 9) '(2 4 6 8 10)))
