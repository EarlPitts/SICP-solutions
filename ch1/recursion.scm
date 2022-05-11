;Recursive Factorial
(define (factorial x)
  (if (= x 1)
    1
    (* x (factorial (- x 1)))))

(factorial 6)

(define (factorial n)
  (define (fact-iter counter product)
    (if (= counter n)
      product
      (* product (fact-iter
                   (+ counter 1)
                   (+ product 1)))))
  (fact-iter 1 1))


;Iterative Factorial
(define (factorial x)
  (define (fact-iter counter product)
    (if (> counter x)
      product
      (fact-iter (+ counter 1)
                 (* product counter))))
  (fact-iter 1 1))

(factorial 5)

;Addition (iterative process)
;Space = O(1)
;Time = O(n)
(define (+ x y)
  (if (= x 0)
    y
    (+ (-1+ x) (1+ y))))

;Addition (recursive process)
;Space = O(n)
;Time = O(n)
(define (+ x y)
  (if (= x 0)
    y
    (1+ (+ (-1+ x) y))))

(+ 4 5)

;Fibonacci
;Space = O(n) (the depth of the tree)
;Time = O(fib(n)) (exponential)
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(fib 10)

;Ackermann
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 2 2)
(A 2 3)
(A 2 4)
(A 2 5)

;Fibonacci Iteratively
(define (fib n)
  (define (iter a b count)
    (if (= count 0)
      b
      (iter (+ a b) a (- count 1))))
  (iter 1 0 n))

 (fib 7) 

;Change counter
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination
                          kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 100)

;Exercise 1.11
;Recursive
(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1))
       (* 2 (f (- n 2)))
       (* 3 (f (- n 3))))))

;Iterative
(define (f n)
  (define (iter a b c count)
    (if (= count 0)
      c
      (iter
        (+ a (* 2 b) (* 3 c))
        a
        b
        (- count 1))))
  (iter 2 1 0 n))


(f 6)

;Exercise 1.12
(define (pascal r c)
  (cond ((= r 1) 1)
        ((or (= c 1) (= c r)) 1)
        (else (+
                (pascal (- r 1) (- c 1))
                (pascal (- r 1) c )))))

(pascal 5 3)

;Eponentiation
;Recursive
(define (expo b n)
  (if (= n 0) 
    1
    (* b (expo b (- n 1)))))

(expo 3 2)

;Iterative
(define (expo b n)
  (define (iter base counter product)
    (if (= counter 0)
      product
      (iter base
            (- counter 1)
            (* product base))))
  (iter b n 1))

(expo 4 3)

;With squaring
(define (fast-expo b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expo b (/ n 2))))
        (else (* b (fast-expo b (- n 1))))))

(fast-expo 3 2)

;Even?
(define (even? n)
  (= (remainder n 2) 0))

(even? 3)
(even? 4)

;Exercise 1.16
(define (fast-expo b n)
  (define (iter base n state)
    (cond ((= n 0) state)
          ((even? n) (iter (square base) (/ n 2) state))
          (else (iter base (- n 1) (* base state)))))
  (iter b n 1))

(fast-expo 3 4)

;Exercise 1.17
(define (fast-mult a b)
  (define (double n) (* 2 n))
  (define (halve n) (/ n 2))
  (cond ((= a 0) 0)
        ((even? a) (double (fast-mult (halve a) b)))
        (else (+ b (fast-mult (- a 1) b)))))

(fast-mult 12 100)

;Exercise 1.19
(define (fast-mult a b)
  (define (double a) (* 2 a))
  (define (halve a) (/ a 2))
  (define (iter a b c)
    (cond ((= a 0) c)
          ((even? a) (iter (halve a) (double b) c))
          (else (iter (- a 1) b (+ c b)))))
  (iter a b 0))

(fast-mult 2 7)

;Peasant Fibonacci
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   1
                   1
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(fib 6)

;Euclid's Algorithm
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(gcd 205 40)

;Prime finder
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

;n is prime is its smallest divisor is itself
(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 5)
(prime? 6)

;Fermat Test
(define (expmod base expo m)
  (cond ((= expo 0) 1)
        ((even? expo)
         (remainder
           (square (expmod base (/ expo 2) m))
           m))
        (else
          (remainder
            (* base (expmod base (- expo 1) m))
            m))))

(expmod 3 8 6)

;Fermat Test
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(fermat-test 16)

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(fast-prime? 561 9)

;Exercise 1.21
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 384160001)

(define (search-for-primes a b)
  (define (iter a b l)
    (cond ((even? a) (iter (+ 1 a) b l))
          ((or (> a b) (= a b)) l)
          ((timed-prime-test a) (iter (+ 2 a) b (cons a l)))
          (else (iter (+ 2 a) b l))))
  (iter a b '()))

(search-for-primes 1000000 1001000)

;Exercise 1.23
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))

(define (next n)
  (if (= n 2)
    3
    (+ 2 n)))

;Exercise 1.25
(define (fast-expo b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expo b (/ n 2))))
        (else (* b (fast-expo b (- n 1))))))

(define (exmpod base expo m)
  (remainder (fast-expt base expo) m))

(expmod 3 7 4)

(define (expmod base expo m)
  (cond ((= expo 0) 1)
        ((even? expo)
         (remainder
           (square (expmod base (/ expo 2) m))
           m))
        (else
          (remainder
            (* base (expmod base (- expo 1) m))
            m))))

;Exercise 1.27
(define (carmichael n)
  (define (test a n)
    (if (= a 0)
      #t
      (and (test (- a 1) n) (= (expmod a n n) (remainder a n)))))
  (test n n))

(carmichael 561)
(carmichael 1105)
(carmichael 1729)

;#########################################
;####### Higher-Order Procedures #########
;#########################################

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (cube x) (* x x x))
(define (inc x) (+ x 1))

(define (sum-cubes a b)
    (sum cube a inc b))

(sum-cubes 1 10)

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

(define (pi-sum a b)
  (define (pi-next n) (+ n 4))
  (define (pi-term n) (/ 1.0 (* n (+ n 2))))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral identity 0 1 0.01)
(integral square 0 1 0.01)
(integral cube 0 1 0.01)

;Exercise 1.29
(define (simpson f a b n)
  (define (h) (/ (- b a) n))
  (define (y k) (f (+ a (* k (h)))))
  (define (l curr)
    (cond ((= curr 0) (y curr))
          ((= curr n) (y curr))
          ((odd? curr) (* 4 (y curr)))
          ((even? curr) (* 2 (y curr)))))
  (* (/ (h) 3) (sum l 0 inc n)))

(simpson cube 0 1 1000)

;Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (plus-two n) (+ n 2))
(sum square 0 plus-two 10)

;Exercise 1.31
;Recursive
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

;Iterative
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* a result))))
  (iter a 1))

(product identity 1 inc 5)

(define (factorial n)
  (product identity 1 inc n))

(factorial 7)

;TODO Pi

;Exercise 1.32
;Recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

;Iterative
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(sum identity 0 inc 10)
(product identity 1 inc 5)

;Exercise 1.33
(define (filtered-accumulate pred combiner null-value term a next b)
  (if (> a b)
    null-value
    (if (pred a)
      (combiner (term a)
                (filtered-accumulate pred combiner null-value term (next a) next b))
      (filtered-accumulate pred combiner null-value term (next a) next b))))

(define (sum-square-prime a b)
    (filtered-accumulate prime? + 0 square a inc b))

(sum-square-prime 2 10)

;Lamda
;These Two are equivalent
(define (add n m) (+ n m))
(define add (lambda (n m) (+ n m)))

(add 1 2)

;Let
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;Exercise 1.34
(define (f g) (g 2))

(f square)
(f f)

;Half-interval Method
(define (average x y) (/ (+ x y) 2))
(define (close-enough? x y) (< (abs (- x y)) 0.00001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))

(search (lambda (x) (+ 3 (* 2 x))) -5 5)

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

(half-interval-method (lambda (x) (* 2 (+ x 3))) -5.0 5.0)
(half-interval-method sin 2.0 4.0)

;Fixpoints
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)
(fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0)

;Never converges
(define (sqrt x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

;With average damping
(define average-damp
  (lambda (f)
    (lambda (x) (average (f x) x))))

((average-damp (lambda (x) (+ 4 x))) 1)

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(sqrt 25)
;Exercise 1.35
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

;Exercise 1.36
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(sqrt 25)

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
(fixed-point (average-damp (lambda (x) (/ (log 1000) (log x)))) 2.0)

;Exercise 1.37
;Recursive
(define (div n d a next b)
  (if (> a b)
    1
    (/ (n a) (+ (d a) (div n d (next a) next b)))))

;Iterative
;TODO This is wrong
(define (div n d a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (/ (n a) (+ (d a) result)))))
    (iter a 1))

(define (cont-frac n d k)
  (div n d 1 1+ k))

(cont-frac (lambda (x) 1.0)
           (lambda (x) 1.0) 
           10)

;Exercise 1.38
(define (d i)
  (cond ((= i 2) 2)
        ((= (remainder (- i 2) 3) 0) (+ 2 (* 2 (/ (- i 2) 3))))
        (else 1.0)))

(cont-frac (lambda (x) 1.0)
           d
           100)

;Exercise 1.39
;TODO Not working, no idea why
(define (div-minus n d a next b)
  (if (> a b)
    1
    (/ (n a) (- (d a) (div-minus n d (next a) next b)))))

(define (tan-cf x k)
  (cont-frac (lambda (i)
               (if (= i 1) x (square x)))
             (lambda (i) (- (* 2 i) 1))
             k))

(tan-cf 5.0 10)

;Procedures as Return Values
((average-damp square) 10)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(cube-root 125)


;Newton's Method
(define (newton f guess)
  (define df (deriv f))
  (fixed-point
    (lambda (x) (- x (/ (f x) (df x))))
    guess))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define dx 0.001)
(define (deriv f)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

((deriv square) 5)
((deriv cube) 5)

(define (sqrt x)
  (newton (lamdba (y) (- x (square y))) 1))
(define (sqrt x)
  (newtons-method
    (lambda (y) (- (square y) x)) 1))

(sqrt 25)

;Abstract the fixed-point
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform
    (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt x)
  (fixed-point-of-transform
    (lambda (y) (- x (square y))) newton-transform 1.0))

;Exercise 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic -3 0 2) -4)
(newtons-method (cubic -3 0 2) 1)
(newtons-method (cubic -3 0 2) 3)

;Exercise 1.41

(define (double f)
  (lambda (x) (f (f x))))

((double 1+) 2)
((double square) 2)

(((double (double double)) 1+) 5)

;Exercise 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square 1+) 6))

;Exercise 1.43
(define (repeated f n)
  (define (identity x) x)
  (define (iter n result)
    (if (= n 0) result (iter (- n 1) (compose f result))))
  (iter n identity))

(define (repeated f n)
  (define (identity x) x)
  (if (= n 0) identity
    (compose f (repeated f (- n 1)))))

((repeated square 2) 5)

;Exercise 1.44
(define (smooth f)
  (define dx 0.0001)
  (define (average3 a b c)
    (/ (+ a b c) 3))
  (lambda (x) (average3 (f (- x dx)) (f x) (f (+ x dx)))))

((smooth square) 2)

(define (repeated-smooth f n)
  ((repeated smooth n) f))

((repeated-smooth square 8) 4)

;Exercise 1.44
;TODO

;Exercise 1.45
;TODO
