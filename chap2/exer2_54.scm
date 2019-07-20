(define (equal? a b)
        (cond ((and (pair? a) (not (pair? b))) #f)
              ((and (not (pair? a)) (pair? b)) #f)
              ((and (not (pair? a)) (not (pair? b)))
               (eq? a b))
              ((and (pair? a) (pair? b))
               (and (equal? (car a) (car b))
                    (equal? (cdr a) (cdr b))))))

; (define (equal? a b)
;   (cond ((and (symbol? a) (list? b)) #f)
;         ((and (list? a) (symbol? b)) #f)
;         ((and (symbol? a) (symbol? b)) (eq? a b))
;         ((and (list? a) (list? b))
;          (cond ((and (null? a) (null? b)) #t)
;                ((and (null? a) (not (null? b))) #f)
;                ((and (not (null? a)) (null? b)) #f)
;                (else (and (equal? (car a) (car b))
;                           (equal? (cdr a) (cdr b))))))
;         (else
;           (error "Invalid type."))))

;; test
(display (equal? '(this is a list) '(this is a list))) ; #t
(newline)
(display (equal? '(this is a list) '(this (is a) list))) ; #f
(newline)
(display (equal? 'a 'a)) ; #t
(newline)
(display (equal? 'a 'b)) ; #f
(newline)
(display (equal? '() '())) ; #t
(newline)
(display (equal? '() 'a)) ; #f
(newline)
; (display (equal? 1 3)) ; error
; (newline)
