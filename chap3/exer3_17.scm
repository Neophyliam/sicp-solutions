(define (count-pairs x)
  (define seen '())

  (define (have-seen? pair)
    (define (iter pair seen-pairs)
      (if (null? seen-pairs)
          #f
          (let ((next-pair (car seen-pairs)))
            (if (eq? pair next-pair)
                #t
                (iter pair (cdr seen-pairs))))))
    (iter pair seen))

  (define (add-to-seen pair)
    (set! seen (cons pair seen)))

  (define (iter x)
    (cond ((not (pair? x)) 0)
          ((have-seen? x) 0)
          (else
            (add-to-seen x)
            (+ (iter (car x))
               (iter (cdr x))
               1))))

  (iter x))


;; test
(define (println x)
  (display x)
  (newline))
(println (count-pairs '(a b c))) ;; expect 3

(define x '(foo))
(define y (cons x x))
(define z (list y))
(println (count-pairs z)) ;; expect 3

(define z (cons y y))
(println (count-pairs z)) ;; expect 3

(define x '(foo))
(set-cdr! x x)
(println (count-pairs x)) ;; expect 1

(define x '())
(println (count-pairs x)) ;; expect 0
