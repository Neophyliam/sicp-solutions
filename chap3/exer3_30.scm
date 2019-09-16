(define (ripple-carry-adder a-list b-list s-list c)
  (let ((c-list (map (lambda (x) (make-wire)) (cdr a-list)))
        (cn (make-wire)))
    (map full-adder
         a-list
         b-list
         (append c-list (list cn))
         s-list
         (cons c c-list))
    'ok))
