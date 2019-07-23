(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (remove-dup set)
  (define (loop rest result)
    (cond ((null? rest) result)
          ((element-of-set? (car rest) result)
           (loop (cdr rest) result))
          (else
            (loop (cdr rest) (cons (car rest) result)))))
  (loop set '()))

;; test
(define (println x)
  (display x)
  (newline))

; union-set
(println (remove-dup (union-set '() '())))
(println (remove-dup (union-set '(a) '())))
(println (remove-dup (union-set '() '(a))))
(println (remove-dup (union-set '(a b) '(c))))
(println (remove-dup (union-set '(a b) '(a))))
(println (remove-dup (union-set '(a b) '(c d))))
(println (remove-dup (union-set '(c d) '(a b))))
(println (remove-dup (union-set '(a b) '(a c))))
(println (remove-dup (union-set '(a b) '(c a))))
(println (remove-dup (union-set '(a b c) '(c b a d e))))
; intersection-set
(println (remove-dup (intersection-set '() '(a))))
(println (remove-dup (intersection-set '(a) '(a))))
(println (remove-dup (intersection-set '(a) '(b))))
(println (remove-dup (intersection-set '(a b) '(a))))
(println (remove-dup (intersection-set '(a b c) '(a b))))
(println (remove-dup (intersection-set '(a b c) '(a b c))))
(println (remove-dup (intersection-set '(a b c) '(b c d e))))
