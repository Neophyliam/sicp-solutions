(define f
  (let ((result 1))
    (lambda (x)
      (set! result (* x result))
      result)))

(display (+ (f 0) (f 1)))
(newline)
