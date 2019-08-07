(define (make-accumulator value)
  (lambda (x)
    (set! value (+ value x))
    value))

;; test
(define (println x)
  (display x)
  (newline))

(define A (make-accumulator 5))
(println (A 10))
(println (A 10))
