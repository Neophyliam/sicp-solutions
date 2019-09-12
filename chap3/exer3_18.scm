(define (has-cycle? list)
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
    (cond ((null? x) #f)
          ((have-seen? x) #t)
          (else
            (add-to-seen x)
            (iter (cdr x)))))

  (iter list))


;; test
(define (println x)
  (display x)
  (newline))
(define x '(a))
(println (has-cycle? x)) ;; expect #f
(define x '(a))
(set-cdr! x x)
(println (has-cycle? x)) ;; expect #t
(define x '(a a))
(set-cdr! (cdr x) x)
(println (has-cycle? x)) ;; expect #t
(define x '(a))
