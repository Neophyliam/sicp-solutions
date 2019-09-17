(define (println x)
  (display x)
  (newline))

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))

    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))

    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
                (set-cdr! rear-ptr new-pair)
                (set! rear-ptr new-pair)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"))
            (else
              (set! front-ptr (cdr front-ptr)))))

    (define (print-queue)
      (define (iter item)
        (cond ((null? item) (println ")"))
              (else
                (display (car item))
                (display " ")
                (iter (cdr item)))))
      (display "(")
      (iter front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) (delete-queue!))
            ((eq? m 'print-queue) (print-queue))
            (else
              (error "Undefined operation -- " m))))
    dispatch))

(define (empty-queue? queue) (queue 'empty-queue?))
(define (front-queue queue) (queue 'front-queue))
(define (insert-queue! queue item)
  ((queue 'insert-queue!) item)
  queue)
(define (delete-queue! queue)
  (queue 'delete-queue!)
  queue)
(define (print-queue queue)
  (queue 'print-queue))


;; test
(define q1 (make-queue))
(insert-queue! q1 'a)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(insert-queue! q1 'b)
(print-queue q1)
