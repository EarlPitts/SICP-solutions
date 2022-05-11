;; ### Streams ###
(define (cons-stream a b)
  (cons a (delay b)))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; Basic functions
(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                        pred
                        (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

;; Delay implementation
;; It's implemented with memo-proc, which memoizes
;; the procedure given to it
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
        (begin (set! result (proc))
               (set! already-run? true)
               result)
        result))))

;; Although the naive solution is to just wrap an expression
;; into a procedure, then call it with force
;; (define delay (lambda () (+ 1 2)))
;; (define (force delayed-object) (delayed-object))

;; Finding first prime in interval
(define (stream-enumerate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumerate-interval (+ low 1) high))))


;; Exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc (map stream-car argstreams))
      (apply stream-map
        (cons proc (map stream-cdr argstreams))))))

;; Exercise 3.51
(define (show x)
  (display-line x)
  x)

(define x (stream-map show
                      (stream-enumerate-interval 0 10)))

;; (stream-map show (stream-enumerate-interval 0 10))
;; (stream-ref x 5)
;; (stream-ref x 7)

;; Exercise 3.52
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
;; (stream-ref y 7)
;; (display-stream z)
;; (display-stream seq)
;; sum

;; ## Infinite Streams ##
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;; Fibonacci sequence
(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
;; (stream-head fibs 10)

;; Primes with Erastothenes sieve
(define (sieve stream)
  (cons-stream
    (stream-car stream)
    (sieve (stream-filter
             (lambda (x)
               (not (divisible? x (stream-car stream))))
             (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

;; Implicit Streams
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

;; Stream of two's powers
(define double (cons-stream 1 (scale-stream double 2)))

;; Stream of primes
(define primes
  (cons-stream
    2
    (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))

;; Exercise 3.53
(define s (cons-stream 1 (add-streams s s)))

;; Exercise 3.54
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

;; Exercise 3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

;; Exercise 3.56
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream
                     s1car
                     (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream
                     s2car
                     (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream
                      s1car
                      (merge (stream-cdr s1)
                             (stream-cdr s2)))))))))

(define odds
  (cons-stream
    1
    (add-streams odds
                (scale-stream ones 2))))

(define evens
  (cons-stream
    0
    (add-streams evens
                (scale-stream ones 2))))

(define S (cons-stream 1 (merge
                           (merge
                             (scale-stream S 2)
                             (scale-stream S 3))
                           (scale-stream S 5))))

;; Exercise 3.57
(define fib
  (memoize
    (lambda (n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (fib (- n 1))
                     (fib (- n 2))))))))


(define (memoize f)
  (let ((table (make-symbol-hash-table)))
    (lambda (x)
      (display (hash-table-keys table))
      (newline)
      (display (hash-table-values table))
      (newline)
      (let ((previously-computed-result
              (hash-table/get table x #f)))
        (or previously-computed-result
            (let ((result (f x)))
              (hash-table-set! table x result)
              result))))))


(define fibs
  (cons-stream
    0
    (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

;; Exercise 3.58
(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

;; Exercise 3.59
;; a
(define (integrate-series s)
  (define (iter s mul)
    (cons-stream
      (* mul (stream-car s))
      (iter (stream-cdr s)
            (/ (numerator mul)
               (1+ (denominator mul))))))
  (iter s 1))

(define (integrate-series s)
  (stream-map * s (stream-map / ones integers)))
(define (integrate-series s)
  (stream-map / s integers))

;; b
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; Exercise 3.60
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                            (mul-series (stream-cdr s2) s1))))

;; Exercise 3.61
;; TODO

;; Exercise 3.62
;; TODO

;; Representing state as streams
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

;; We can't use let here for guesses
(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;; Sequence accelerator, for faster convergence
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))   ;Sn-1
        (s1 (stream-ref s 1))   ;Sn
        (s2 (stream-ref s 2)))  ;Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;; Creating a stream of streams (tableau) with an accelerator
(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))



(stream-head (accelerated-sequence euler-transform pi-stream) 10)
        
