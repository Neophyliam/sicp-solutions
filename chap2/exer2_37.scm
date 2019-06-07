(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      (list)
      (cons (accumulate op init
                        (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose m)
  (accumulate-n cons (list) m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-row)
           (map (lambda (n-col) (dot-product m-row n-col)) cols))
         m)))

;; test
(define vec1 (list 1 2 3))
(define vec2 (list 4 5 6))
(display (dot-product vec1 vec2))
(newline)
(define mat1 (list (list 1 2 3)
                   (list 4 5 6)
                   (list 7 8 9)))
(display (transpose mat1))
(newline)
(display (matrix-*-vector mat1 vec1))
(newline)
(display (matrix-*-matrix mat1 (list (list 1)
                                     (list 2)
                                     (list 3))))
(newline)
(define mat2 (list (list 1 0 0)
                   (list 1 1 0)
                   (list 0 0 1)))
(display (matrix-*-matrix mat1 mat2))
(newline)
