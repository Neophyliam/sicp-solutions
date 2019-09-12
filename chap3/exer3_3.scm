(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch secret method)
    (if (eq? secret passwd)
        (cond ((eq? method 'withdraw) withdraw)
              ((eq? method 'deposit) deposit)
              (else
                (error "Unknown request -- MAKE-ACCOUNT" method)))
        (lambda (x) "Incorrect password")))
  dispatch)


;; test
(define (println x)
  (display x)
  (newline))

(define acc (make-account 100 'secret-password))

(println ((acc 'secret-password 'withdraw) 40))
(println ((acc 'secret-password 'deposit) 40))
(println ((acc 'some-other-password 'deposit) 50))
