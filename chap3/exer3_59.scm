;; a)
(define (integrate-series stream)
  (define (iter number rest)
    (cons-stream (/ (stream-car rest) number)
                 (iter (+ number 1) (stream-cdr rest))))
  (iter 1 stream))

;; b)
(define cosine-series
  (cons-stream 1 (scale-stream
                   -1
                   (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
