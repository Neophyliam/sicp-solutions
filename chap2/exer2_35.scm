(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (not (pair? x))
                             1
                             (count-leaves x)))
                       t)))

;; test
(define tree (list 1 2 (list 3 (list 4))))
(display (count-leaves tree))
(newline)
