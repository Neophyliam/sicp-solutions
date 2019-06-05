(define (tree-map func tree)
  (cond ((null? tree) (list))
        ((not (pair? tree)) (func tree))
        (else
          (cons (tree-map func (car tree))
                (tree-map func (cdr tree))))))

;; test
(define (square x) (* x x))
(define (square-tree tree) (tree-map square tree)) 
(display (square-tree (list 1
                      (list 2 (list 3 4) 5)
                      (list 6 7))))
(newline)
