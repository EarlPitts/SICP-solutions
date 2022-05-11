;; ### Concurrency ###
;; Execute two processes parallelly, which can result
;; in multiple differetn outcomes
(define x 10)
(parallel-execute
  (lambda () (set! x (* x x)))
  (lambda () (set! x (+ x 1))))

;; Serialization
;; Adds the two procedures into the same serialization "set",
;; preventing them to execute concurrently
(define x 10)
(define s (make-serializer))
(parallel-execute
  (s (lambda () (set! x (* x x))))
  (s (lambda () (set! x (+ x 1)))))

;; Serialized Bank Account
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))

;; Providing the Serializer
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

;; Serializtion is the responsibility of the caller
(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'desposit)))
    ((s d) amount)))

;; Serialized exchanging
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))


;; Serializer implementation
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

;; Mutex implementation
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire))) ;retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))
(define (test-and-set! cell)
  (if (car cell) true (begin (set-car! cell true) false)))

;; Exercise 3.47
;; TODO

(define (make-semaphore n)
  (define (dispatcher m)
    (cond ((eq? m 'acquire)
           (if (= num 0)
             (the-mutex 'acquire)
             (set! num (- num 1)))) ;retry
          ((eq? m 'release) (set! num (+ num 1)))))
  dispatcher)

;; Exercise 3.48
;; TODO

;; Exercise 3.49
;; TODO
