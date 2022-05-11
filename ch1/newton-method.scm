;Square Root
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)

(define (if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter2 guess x)
  (if (< (abs (- guess (improve guess x))) 0.001)
    guess
    (sqrt-iter (improve guess x) x)))

(sqrt-iter 1.0 166)
(sqrt-iter2 1.0 166)

; Cube root
(define (cube x) (* x x x))

(define (improve-cube guess x)
  (/ 
    (+ 
      (/ x (square guess)) 
      (* 2 guess))
    3)))

(define (good-enough-cube? guess x)
  (< (abs (- x (cube guess))) 0.001))

(define (cube-root guess x)
  (if (good-enough-cube? guess x)
    guess
    (cube-root (improve-cube guess x) x)))


(cube-root 1.0 27)

; Block Structure
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt 9)
