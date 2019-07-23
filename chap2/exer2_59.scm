(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (union-set set1 set2)
  (accumulate adjoin-set set1 set2))

;; test
(define (println x)
  (display x)
  (newline))

(println (union-set '() '()))
(println (union-set '(a) '()))
(println (union-set '() '(a)))
(println (union-set '(a b) '(c)))
(println (union-set '(a b) '(a)))
(println (union-set '(a b) '(c d)))
(println (union-set '(c d) '(a b)))
(println (union-set '(a b) '(a c)))
(println (union-set '(a b) '(c a)))
(println (union-set '(a b c) '(c b a d e)))
