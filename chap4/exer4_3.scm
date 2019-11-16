(define (tag tagged-exp) (car exp))

(define (tagged? exp)
  (and (pair? exp) (symbol? (tag exp))))

(define evaluator-list (list))

(define (register-evaluator tag evaluator)
  (set! evaluator-list (cons (list tag evaluator) evaluator-list))
  'done)

(define (get-evaluator exp)
  (define (iter evaluator-list)
    (cond ((null? evaluator-list)
           (error "Unknown expression type -- GET-EVALUATOR" exp))
          ((eq? (tag exp) (caar evaluator-list))
           (cadar evaluator-list))
          (else
            (iter (cdr evaluator-list)))))
  (iter evaluator-list))

;; Register evaluators
(register-evaluator 'quote (lambda (exp env) (text-of-quotation exp)))
(register-evaluator 'set! eval-assignment)
(register-evaluator 'define eval-definition)
(register-evaluator 'if eval-if)
(register-evaluator 'lambda
                    (lambda (exp env)
                      (make-procedure (lambda-parameters exp)
                                      (lambda-body exp)
                                      env)))
(register-evaluator 'begin
                    (lambda (exp env)
                      (eval-sequence (begin-actions exp) env)))
(register-evaluator 'cond
                    (lambda (exp env)
                      (eval (cond->if exp) env)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (loopup-variable-value exp env))
        ((tagged? exp) ((get-evaluator exp) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "UnKnown expression type -- EVAL" exp))))
