(define (make-monitored func)
  (let ((count 0))
    (define (dispatch s)
      (cond ((eq? s 'how-many-calls?) count)
            ((eq? s 'reset-count) (set! count 0))
            (else
              (set! count (+ count 1))
              (func s))))
    dispatch))


;; test
(define (println x)
  (display x)
  (newline))

(define s (make-monitored sqrt))
(println (s 100))
(println (s 'how-many-calls?))
(s 'reset-count)
(println (s 'how-many-calls?))
