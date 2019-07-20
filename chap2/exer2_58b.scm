(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define operator-precedence
  '((+ . 0)
    (* . 1)
    (maxop . 1000)))

(define (operator? a)
  (define (loop op-table)
    (cond ((null? op-table) #f)
          ((eq? a (caar op-table)) #t)
          (else (loop (cdr op-table)))))
  (loop operator-precedence))

(define (precedence a)
  (define (loop op-table)
    (cond ((null? op-table)
           (error "Operator not defined -- PRECEDENCE:" a))
          ((eq? a (caar op-table)) (cdar op-table))
          (else (loop (cdr op-table)))))
  (loop operator-precedence))

(define (min-op op1 op2)
  (if (< (precedence op1) (precedence op2))
      op1
      op2))

(define (least-op exp)
  (accumulate (lambda (a b)
                (if (operator? a)
                    (min-op a b)
                    b))
              'maxop
              exp))

(define (prefix sym li)
  (if (or (null? li) (eq? sym (car li)))
      '()
      (cons (car li) (prefix sym (cdr li)))))

(define (sum? x)
  (and (pair? x) (eq? (least-op x) '+)))

(define (addend s)
  (let ((left-hand (prefix '+ s)))
    (if (= (length left-hand) 1)
        (car left-hand)
        left-hand)))

(define (augend s)
  (let ((right-hand (cdr (memq '+ s))))
    (if (= (length right-hand) 1)
        (car right-hand)
        right-hand)))

(define (product? x)
  (and (pair? x) (eq? (least-op x) '*)))

(define (multiplier p)
  (let ((left-hand (prefix '* p)))
    (if (= (length left-hand) 1)
        (car left-hand)
        left-hand)))

(define (multiplicand p)
  (let ((right-hand (cdr (memq '* p))))
    (if (= (length right-hand) 1)
        (car right-hand)
        right-hand)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (multiplicand exp)
                                 (deriv (multiplier exp) var))))
        (else
          (error "unknown expression type -- DERIV" exp))))

;; test
(define (println x)
  (display x)
  (newline))

(display '(deriv (x + 3) x))
(display " => ")
(println (deriv '(x + 3) 'x))
(display '(deriv (x * y) x))
(display " => ")
(println (deriv '(x * y) 'x))
(display '(deriv ((x * y) * (x + 3)) x))
(display " => ")
(println (deriv '((x * y) * (x + 3)) 'x))
(display '(deriv (x * 1) x))
(display " => ")
(println (deriv '(x * 1) 'x))
(display '(deriv (x + 3 * (x + y + z)) x))
(display " => ")
(println (deriv '(x + 3 * (x + y + z)) 'x))
(display '(deriv (x * x * y * x) x))
(display " => ")
(println (deriv '(x * x * y * x) 'x))
