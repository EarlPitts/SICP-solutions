(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
    (begin (set! balance (- balance amount))
           balance)
    "Insufficient funds"))

;Assignment
(set! balance 30)

;Sequence of expressions
(begin (exp1) (exp2) (exp3))

;Local state
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))

(new-withdraw 20)

;Closure
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds")))

(define w1 (make-withdraw 100))
(define w2 (make-withdraw 100))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))

;Exercise 3.1
(define (make-accumulator value)
  (lambda (amount)
    (begin (set! value (+ value amount))
      value)))

;Exercise 3.2
(define (make-monitored f)
  (let ((counter 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) counter)
            ((eq? x 'reset-count) (set! counter 0))
            (else
              (begin (set! counter (1+ counter))
                     (f x)))))))

;Exercise 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m p)
      (cond ((not (eq? p password)) (error "Wrong password"))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch)

;Exercise 3.4
(define (make-account balance password)
  (let ((incorrect-times 0))
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-cops) (error "Calling the cops"))
    (define (dispatch m p)
      (cond ((not (eq? p password))
             (if (= incorrect-times 6)
               (call-cops)
               (begin (set! incorrect-times (1+ incorrect-times))
                      (error "Wrong password" incorrect-times))))
            ((eq? m 'withdraw) 
             (begin (set! incorrect-times 0)
                    withdraw))
            ((eq? m 'deposit)
             (begin (set! incorrect-times 0)
                    deposit))
            (else (begin 
                    (set! incorrect-times 0)
                    (error "Unknown request: MAKE-ACCOUNT" m)))))
  dispatch))

(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

;Monte-Carlo
(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
            (iter (- trials-remaining 1)
                  trials-passed))))
  (iter trials 0))

;Exercise 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (unit-circle x y)
  (<= (+ (pow (- x 0)) 
         (pow (- y 0)))
      1))

(define (area-of-rec lower upper)
  (* (- (car upper) (car lower))
     (- (cadr upper) (cadr lower))))

(define (pow x) (* x x))

(define (estimate-integral P lower upper trials)
    (define (experiment)
      (P (random-in-range (car lower) (car upper))
         (random-in-range (cadr lower) (cadr upper))))
    (* (area-of-rec lower upper) (monte-carlo trials experiment)))
                          
(estimate-integral unit-circle (list -1. -1.) (list 1. 1.) 1000)

;Exercise 3.6
(define rand
  (let ((a 3)
        (b 4)
        (m 5)
        (val 0))
    (define (generate)
      (begin (set! val (modulo (+ (* a val) b) m))
             val))
    (define (reset value)
      (set! val value))
    (define (dispatch mes)
      (cond ((eq? mes 'generate) (generate))
            ((eq? mes 'reset) reset)))
  dispatch))

(define (rand-update x)
  (let ((a 3)
        (b 4)
        (m 5))
    (modulo (+ (* a x) b) m)))

(define rand 
  (let ((x 0)) 
    (define (dispatch message) 
      (cond ((eq? message 'generate) 
             (begin (set! x (rand-update x)) 
                    x)) 
            ((eq? message 'reset) 
             (lambda (new-value) (set! x new-value))))) 
    dispatch))

;Factorial with assignments
(define (factorial n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
        product
        (begin (set! product (* counter product))
               (set! counter (+ counter 1))
               (iter))))
    (iter)))

;Exercise 3.7
(define (make-account balance password)
    (define (withdraw amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (dispatch m p)
      (cond ((not (eq? p password)) (error "Wrong password"))
            ((eq? m 'auth) #t)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
  dispatch)

(define (make-joint acc orig-pass joint-pass)
  (define (withdraw amount)
    ((acc 'withdraw orig-pass) amount))
  (define (deposit amount)
    ((acc 'deposit orig-pass) amount))
  (define (dispatch m p)
    (cond ((not (eq? p joint-pass)) (error "Wrong password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) withdraw)))
  dispatch)

;Exercise 3.8
(define f
  (let ((state 0))
    (lambda (x)
      (if (= x 0)
        (begin (set! state -1) 0)
        (+ x state)))))

 (define f 
   (let ((count 1)) 
     (lambda (x)  
        (set! count (* count x)) 
        count))) 


;Exercise 3.10
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

;Mutable Data
(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))

;Exercise 3.16
(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

(begin
  (define l1 (list 1 2 3))
  (count-pairs l1))

(begin
  (define l2 (list 1 2 3))
  (set-car! l2 (cddr l2))
  (count-pairs l2))

(begin
  (define l3 (list 1 2 3))
  (set-car! l3 (cdr l3))
  (set-car! (cdr l3) (cddr l3))
  (count-pairs l3))

(begin
  (define l4 (list 1 2 3))
  (set-car! l4 l4)
  (count-pairs l4))

;Exercise 3.17
(define (in l x)
  (cond ((null? l) #f)
        ((eq? x (car l)) #t)
        (else (in (cdr l) x))))        

(define (count-pairs x)
  (let ((traversed '()))
    (define (iter x)
      (if (or (not (pair? x)) (in traversed x))
        0
        (begin (display traversed) (newline)
               (set! traversed (cons x traversed))
               (+ (iter (car x))
                  (iter (cdr x))
                  1))))
    (iter x)))

(begin
  (define l3 (list 1 2 3))
  (set-car! l3 (cdr l3))
  (set-car! (cdr l3) (cddr l3))
  (count-pairs l3))

;Exercise 3.18
(define (check x)
  (let ((traversed '()))
    (define (iter x)
      (cond ((null? x) #f)
            ((in traversed x) #t)
            (else
              (begin (display traversed) (newline)
                     (set! traversed (cons x traversed))
                     (iter (cdr x))))))
    (iter x)))

(begin
  (define l3 (list 1 2 3))
  (set-car! l3 (cdr l3))
  (set-car! (cdr l3) l3)
  (check l3))

;Mutable Data with Assignment
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch op)
    (cond ((eq? op 'car) x)
          ((eq? op 'cdr) y)
          ((eq? op 'set-car!) set-x!)
          ((eq? op 'set-cdr!) set-y!)
          (else error "Error")))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z v) ((z 'set-car!) v) z)
(define (set-cdr! z v) ((z 'set-cdr!) v) z)

(car (set-car! (cons 1 2) 7))

;Queue
(define (make-queue) (cons '() '()))
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue")
    (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
        (cond ((empty-queue? queue)
               (set-front-ptr! queue new-pair)
               (set-rear-ptr! queue new-pair)
               queue)
              (else
                (set-cdr! (rear-ptr queue) new-pair)
                (set-rear-ptr! queue new-pair)
                queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE called on empty queue"))
        (else (set-front-ptr! queue (cdr (front-ptr queue)))
              queue)))

;Exercise 3.21
(define (print-queue queue)
  (if (empty-queue? queue)
    (display '())
    (display (front-ptr queue))))

;Exercise 3.22
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with empty queue")
        (car front-ptr)))
    (define (insert-queue item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair))))
                (display-queue))
    (define (delete-queue)
      (cond ((empty-queue?)
             (error "DELETE on empty queue"))
            (else
             (set-front-ptr! (cdr front-ptr))
             (display-queue))))
    (define (display-queue)
      (if (empty-queue?)
        (display '())
        (display front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) (set-front-ptr!))
            ((eq? m 'set-rear-ptr!) (set-rear-ptr!))
            ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'insert-queue) insert-queue)
            ((eq? m 'delete-queue) (delete-queue))
            ((eq? m 'display-queue) (display-queue))))
    dispatch))

;Exercise 3.23
(define (make-dequeue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-dequeue?) (null? front-ptr))
    (define (front-dequeue)
      (if (empty-dequeue?)
        (error "FRONT called with empty queue")
        (car front-ptr)))
    (define (rear-dequeue)
      (if (empty-dequeue?)
        (error "FRONT called with empty queue")
        (car rear-ptr)))
    (define (front-insert-dequeue! item)
      (cond ((empty-dequeue?)
             (let ((new-pair (cons item '())))
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)))
             (else
               (let ((new-pair (cons item front-ptr)))
                 (set-front-ptr! new-pair))))
      (display-dequeue))
    (define (rear-insert-dequeue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-dequeue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair))
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair))))
      (display-dequeue))
    (define (front-delete-dequeue!)
      (cond ((empty-dequeue?)
             (error "DELETE on empty queue"))
            (else
             (set-front-ptr! (cdr front-ptr))
             (display-queue))))
    (define (rear-delete-dequeue!)
      (cond ((empty-dequeue?)
             (error "DELETE on empty queue"))
            (else
             (set-rear-ptr! (cdr front-ptr))
             (display-queue))))
    (define (display-dequeue)
      (if (empty-dequeue?)
        (display '())
        (display front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) (set-front-ptr!))
            ((eq? m 'set-rear-ptr!) (set-rear-ptr!))
            ((eq? m 'empty-dequeue?) (empty-dequeue?))
            ((eq? m 'front-dequeue) (front-dequeue))
            ((eq? m 'rear-dequeue) (rear-dequeue))
            ((eq? m 'front-insert-dequeue!) front-insert-dequeue!)
            ((eq? m 'rear-insert-dequeue!) rear-insert-dequeue!)
            ((eq? m 'delete-queue) (delete-queue))
            ((eq? m 'display-dequeue) (display-dequeue))))
    dispatch))

;;Tables
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))
(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define a '(*table* (a . 1) (b . 2) (c . 3)))
(lookup 'c a)

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value)
                      (cdr table)))))
  'ok)

(insert! 'd 4 a)
(lookup 'd a)

(define (make-table)
  (list '*table*))

;;2D Tables
(define (lookup key-1 key-2 table)
  (let ((subtable
          (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          #f))
      #f)))


(define a (list '*table* (cons 'd b)))
(define a `(*table* (d . ,b)))
(define b '((a . 1) (b . 2) (c . 3)))
(lookup 'd 'b a)

;;Table as procedure
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
              (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
              (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record
                  (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable 
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1 (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (display-table)
      (display local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'display-proc) (display-table))
            (else (error "Unknow operation: TABLE" m))))
    dispatch))

(define t (make-table))

((t 'insert-proc!) 'cheese 'cheddar 9)
((t 'insert-proc!) 'horse 'wlaker 7)
(t 'display-proc)
((t 'lookup-proc) 'cheese 'cheddar)

;;Data Directed programming procedures
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'sajt 'jancsi 7)
(put 'lo 'bela 7)
(get 'sajt 'jancsi)

;;Exercise 3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
              (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (insert! key-1 key-2 value)
      (let ((subtable
              (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record
                  (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable 
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1 (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (display-table)
      (display local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'display-proc) (display-table))
            (else (error "Unknow operation: TABLE" m))))
    dispatch))

;;Exercise 3.25
(define (make-table)
    (define (entry tree) (car tree))
    (define (left-branch tree) (cadr tree))
    (define (right-branch tree) (caddr tree))
    (define (make-tree entry left right)
      (list entry left right))

    (define (make-record key value) (cons key value))
    (define (key record) (car record))
    (define (value record) (cdr record))

  (let ((local-table '()))

    (define (lookup given-key)
      (define (lookup-iter tree)
        (cond ((null? tree) false)
              ((equal? given-key (key (entry tree)))
               (value (entry tree)))
              ((< given-key (key (entry tree)))
               (lookup-iter (left-branch tree)))
              ((> given-key (key (entry tree)))
               (lookup-iter (right-branch tree)))))
      (lookup-iter local-table))

    (define (insert! given-key given-value)
      (define (insert!-iter tree)
        (cond ((null? tree) (set! local-table (make-tree (make-record given-key given-value) '() '())))
              ((equal? given-key (key (entry tree))) (error "Key already defined"))
              ((< given-key (key (entry tree)))
               (if (null? (left-branch tree))
                 (set-car! (cdr tree) (make-tree (make-record given-key given-value) '() '()))
                 (insert-!iter (left-branch tree))))
              ((> given-key (key (entry tree)))
               (if (null? (right-branch tree))
                 (set-car! (cddr tree) (make-tree (make-record given-key given-value) '() '()))
                 (insert!-iter (right-branch tree))))))
      (insert!-iter local-table))

    (define (display-table)
      (display local-table))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'display-proc) (display-table))
            (else (error "Unknow operation: TABLE" m))))

    dispatch))

(define t (make-table))
((t 'lookup-proc) 7)
((t 'insert-proc!) 9 'c)
((t 'insert-proc!) 7 'b)
((t 'display-proc))

;Exercise 3.27
;Memoization
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (table 'display-proc)
      (newline)
      (let ((previously-computed-result
              ((table 'lookup-proc) x)))
        (or previously-computed-result
            (let ((result (f x)))
              ((table 'insert-proc!) x result)
              result))))))


(define memo-fib
  (memoize
    (lambda (n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (memo-fib (- n 1))
                     (memo-fib (- n 2))))))))

;Electric Circuit Simulation
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input) 'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (logical-and a b)
  (cond ((and (eq? a 1) (eq? b 1)) 1)
        ((or (eq? a 0) (eq? b 0)) 0)
        (else (error "Invalid signal" a b))))

(define (logical-or a b)
  (cond ((and (eq? a 0) (eq? b 0)) 0)
        ((or (eq? a 1) (eq? b 1)) 1)
        (else (error "Invalid signal" a b))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay
        and-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

;Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) (get-signal a2))))
      (after-delay
        or-gate-delay
        (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

;Exercise 3.29
(define (or-gate a1 a2 output)
  (let ((b (make-wire)) (c (make-wire)) (d (make-wire)))
    (inverter a1 b)
    (inverter a2 c)
    (and-gate b c d)
    (inverter d output)
    'ok))

;Exercise 3.30
;Ripple-Carry Adder
(define (ripple-carry-adder a b s c)
  (if (null? a)
    'ok
      (let ((c-out make-wire))
        (full-adder (car a) (car b) (car s) c c-out)
        (ripple-carry-adder (cdr a) (cdr b) (cdr s) c-out))))

;; ### Wires ###
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures
        (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Uknown operation: WIRE" m))))
  dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin ((car procedures))
           (call-each (cdr procedures)))))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; ### Agenda ####
;; Contains the schedule of things to do
;; This is where the delay thing is implemented

;; Time Segments
;; Consists of a number (the time)
;; and a queue of procedures that are
;; scheduled at that time
;; '(7 (8 (...)) (11 (...)) (23 (...)))
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))

;; Adding/removing time segments to/from the agenda
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr!
            segments
            (cons (make-new-time-segment time action)
                  (cdr segments)))
          (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments!
        agenda
        (cons (make-new-time-segment time action)
              segments))
      (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty: FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda
                         (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))



;; Delay
;; Adds the action to the agenda with specified delay
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

;; Drives the simulation itself
(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

;; Simulation
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

;; Creating the agenda
(define the-agenda (make-agenda))

;; Specifying delays
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

;; Creating wires
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

;; Attaching probes
(probe 'sum sum)
(probe 'carry carry)

;; Attaching wires to half-adder
(half-adder input-1 input-2 sum carry)
;; Set new signal on input
(set-signal! input-1 1)
;; Run simulation
(propagate)

(set-signal! input-2 1)
(propagate)

;; ### Constraint Propagation ###
;; This system is similar to the electric circut system,
;; but here what propagates is not electric signals,
;; but "constraints" about some expression.
;; Each time the systems gets some new information about one of,
;; it's subexpressions, it propagates it through its "connectors",
;; which in turn propagate it to their connectors, and so on.

;; The main advantage of this system is the "nondirectioinality"
;; of its computation. You can set any n-1 of its n inputs, and get
;; the last one.

;; Sample Usage
(define C (make-connector))
(define F (make-connector))

;; Definition of a system that converts between celsius and fahrenheit
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

(celsius-fahrenheit-converter C F)

;; Setting probes on "outputs"
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

;; The third argument tells the 
;; system that it was set by the user
(set-value! C 25 'user)

;; This will result in a contradiction
(set-value! F 212 'user)

;; You can make the system forget some value to reuse it
(forget-value! C 'user)

;; Implementation
;; Adder
(define (adder a1 a2 sum)
  ;; Called when the constraint is informed about a new value
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                      (- (get-value sum) (get-value a2))
                      me))))
  ;; Called when the contraint is informed that it should forget a value
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown reqest: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

;; Multiplier
(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER"
                       request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline) (display "Probe: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value) (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

;; Connectors
(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin (set! informant false)
               (for-each-except retractor
                                inform-about-no-value
                                constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints
          (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure l)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop l))

;; Interface for the constraint
(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

;; Interface for the connector
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


;; Exercise 3.33
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(probe "A" a)
(probe "B" b)
(probe "C" c)

(define (average a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier c v u)
    (constant 2 v)
    'ok))

(average a b c)

;; Exercise 3.34
(define a (make-connector))
(define b (make-connector))
(probe "A" a)
(probe "B" b)
(define (squarer a b)
  (multiplier a a b))
(squarer a b)

(set-value! a 12 'user)
(set-value! b 100 'user)

(forget-value! a 'user)
(forget-value! b 'user)

;; Exercise 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
      (if (< (get-value b) 0)
        (error "square less 0: SQUARER"
               (get-value b))
        (set-value! a (sqrt (get-value b)) me))
      (if (has-value? a)
        (set-value! b (square (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: SQUARER"
                       request))))
  (connect a me)
  (connect b me)
  me)

(define a (make-connector))
(define b (make-connector))
(probe "A" a)
(probe "B" b)
(set-value! a 12 'user)
(set-value! b 100 'user)
(forget-value! a 'user)

(squarer a b)

;; Exercise 3.37
;; TODO
