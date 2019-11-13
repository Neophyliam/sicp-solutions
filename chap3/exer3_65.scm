(define (ln2-terms n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-terms (+ n 1)))))

;; first stream
(define ln2-stream
  (partial-sums (ln2-terms 1)))

;; second stream
(define euler-transformed-stream
  (euler-transform ln2-stream))

;; third stream
(define recursive-transformed-stream
  (accelerated-sequence euler-transform ln-stream))
