(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) (list) sequence))
(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) (list) sequence))

;; test
(define seq (list 1 2 3 4 5))
(display (reverse-right seq))
(newline)
(display (reverse-left seq))
(newline)
