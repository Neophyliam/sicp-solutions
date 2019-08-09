(define (monte-carlo trails experiment)
  (define (iter trails-remaining trails-passed)
    (cond ((= trails-remaining 0)
           (/ trails-passed trails))
          ((experiment)
           (iter (- trails-remaining 1)
                 (+ trails-passed 1)))
          (else
            (iter (- trails-remaining 1) trails-passed))))
  (iter trails 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trails)
  (let ((len (abs (- x1 x2)))
        (width (abs (- y1 y2))))
    (let ((area (* len width)))
      (* area (monte-carlo trails P)))))

(define (in-circle? x y)
  (<= (+ (* x x) (* y y)) 1.0))

(define (experiment)
  (in-circle? (random-in-range -1.0 1.0)
              (random-in-range -1.0 1.0)))

(define (estimate-pi trails)
  (estimate-integral experiment -1.0 1.0 -1.0 1.0 trails))


;; test
(define (println x)
  (display x)
  (newline))

(println (estimate-pi 90000))
