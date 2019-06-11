(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

(define (make-segment v1 v2) (cons v1 v2))

(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame) (start-segment segment))
          ((frame-coord-map frame) (end-segment segment))))
      segment-list)))

(define bottom-left (make-vect 0 0))
(define bottom-right (make-vect 1 0))
(define up-left (make-vect 0 1))
(define up-right (make-vect 1 1))

(define frame-painter-segments
  (list (make-segment bottom-left bottom-right)
        (make-segment bottom-right up-right)
        (make-segment up-right up-left)
        (make-segment up-left bottom-left)))
(define frame-painter (segments->painter frame-painter-segments))

(define x-painter-segments
  (list (make-segment bottom-left up-right)
        (make-segment bottom-right up-left)))
(define x-painter (segments->painter x-painter-segments))

(define bottom-mid (make-vect 0.5 0))
(define up-mid (make-vect 0.5 1))
(define left-mid (make-vect 0 0.5))
(define right-mid (make-vect 1 0.5))

(define diamond-painter-segments
  (list (make-segment bottom-mid right-mid)
        (make-segment right-mid up-mid)
        (make-segment up-mid left-mid)
        (make-segment left-mid bottom-mid)))
(define diamond-painter (segments->painter diamond-painter-segments))
