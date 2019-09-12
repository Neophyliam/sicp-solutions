(define (has-cycle? l)

  (define (safe-cdr li)
    (cond ((null? li) '())
          ((null? (cdr li)) '())
          (else (cdr li))))

  (define (safe-cddr li)
    (safe-cdr (safe-cdr li)))

  (define (iter a b)
    (cond ((null? b) #f)
          ((eq? a b) #t)
          (else (iter (safe-cdr a) (safe-cddr b)))))

  (iter l (safe-cdr l)))


;; test
(define (println x)
  (display x)
  (newline))
(define x '(a b c d e f))
(println (has-cycle? x)) ;; expect #f
(define x '(a))
(set-cdr! x x)
(println (has-cycle? x)) ;; expect #t
(define x '(a a))
(set-cdr! (cdr x) x)
(println (has-cycle? x)) ;; expect #t
(define x '(a))
