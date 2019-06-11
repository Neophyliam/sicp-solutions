(define (below painter1 painter2)
  (let ((painter-left (rotate270 painter1))
        (painter-right (rotate270 painter2)))
    (rotate90 (beside painter-left painter-right))))
