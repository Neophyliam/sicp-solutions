(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
                (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (encode message tree)
  (if (null? message) '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (define (encode-1 symbol current-branch)
    (cond ((leaf? current-branch) '())
          (else
            (let ((left (left-branch current-branch))
                  (right (right-branch current-branch)))
              (if (element-of-set? symbol (symbols left))
                  (cons 0 (encode-1 symbol left))
                  (cons 1 (encode-1 symbol right)))))))

  (if (not (element-of-set? symbol (symbols tree)))
      (error "bad symbol -- ENCODE-SYMBOL" symbol)
      (encode-1 symbol tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge node-set)
  (cond ((null? node-set)
         (error "node-set is empty -- SUCCESSIVE-MERGE"))
        ((= (length node-set) 1) (car node-set))
        (else
          (let ((first-node (car node-set))
                (second-node (cadr node-set)))
            (successive-merge
              (adjoin-set (make-code-tree first-node second-node)
                          (cddr node-set)))))))

;; test
(define (println x)
  (display x)
  (newline))

(define huffman-tree (generate-huffman-tree '((A 2)
                                              (NA 16)
                                              (BOOM 1)
                                              (SHA 3)
                                              (GET 2)
                                              (YIP 9)
                                              (JOB 2)
                                              (WAH 1))))
(println huffman-tree)

(define encoded (encode '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM) huffman-tree))
(println encoded)
(println (length encoded))
