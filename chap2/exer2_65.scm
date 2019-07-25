(define (union-set set1 set2)
  (list->tree (union-set-list (tree->list-2 set1)
                              (tree->list-2 set2))))

(define (intersection-set set1 set2)
  (list->tree (intersection-set-list (tree->list-2 set1)
                                     (tree->list-2 set2))))
