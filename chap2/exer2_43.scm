(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else
          (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      (list)
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define empty-board (list))

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))

(define (same-row? pos-1 pos-2)
  (= (car pos-1) (car pos-2)))

(define (same-diag? pos-1 pos-2)
  (= (abs (- (car pos-2) (car pos-1)))
     (abs (- (cadr pos-2) (cadr pos-1)))))

(define (conflict? new-position rest-positions)
  (if (null? rest-positions)
      #f
      (or (or (same-row? new-position (car rest-positions))
              (same-diag? new-position (car rest-positions)))
          (conflict? new-position (cdr rest-positions)))))

(define (safe? k positions)
  (let ((reversed-positions (reverse positions)))
    (let ((new-position (car reversed-positions))
          (rest-positions (cdr reversed-positions)))
      (not (conflict? new-position rest-positions)))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) (safe? k positions))
          (flatmap
            (lambda (new-row)
              (map (lambda (rest-of-queens)
                     (adjoin-position new-row k rest-of-queens))
                   (queen-cols (- k 1))))
            (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;; test
(for-each (lambda (solution)
            (display solution)
            (newline))
          (queens 6))
(display (length (queens 6)))
(newline)
