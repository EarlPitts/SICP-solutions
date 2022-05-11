;Complex Number system
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

;Rectangular representation
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))
(define (angle z)
  (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;Polar representation
(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))
(define (make-from-mag-ang r a) (cons r a))

;Tags
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;Data Directed Approach
;Have a table that pairs operations and types
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types: APPLY-GENERIC"
          (list op type-tags))))))


;Exercise 2.73
;TODO
; (define (deriv exp var)
;   (cond ((number? exp) 0)
;         ((variable? exp) (if (same-variable? exp var) 1 0))
;         (else ((get 'deriv (operator exp))
;                (operands exp) var))))
; (define (operator exp) (car exp))
; (define (operand exp) (cdr exp))

; (get 'deriv '+ (1 2) var)

; (define (install-deriv-package)
;   (define (make-sum a b) (cons a b))
;   (define (addend s) (car s))
;   (define (augend s) (cdr s))
;   (define deriv-sum 
;     (lambda (operands var)
;       (make-sum (deriv (addend exp) var)
;                 (deriv (augend exp) var))))

;   (define (make-product a b) (cons a b))
;   (define (multiplier p) (car p))
;   (define (multiplicand p) (cdr p))
;   (define deriv-product 
;     (lambda (p var)
;       (make-sum (make-product
;                   (multiplier 
                

;   (put 'deriv '+ deriv-sum)
;   'done)

;Exercise 2.74
;TODO

;Message Passing
;Data is represented as a procedure,
;that accepts an operator as argument,
;then presents the primitive data as needed,
;for that operation
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;Exercise 2.75
(define (make-from-mag-ang r a)
    (define (dispatch op)
      (cond ((eq? op 'real-part) (* r (cos a)))
            ((eq? op 'imag-part) (* r (sin a)))
            ((eq? op 'magnitude) r)
            ((eq? op 'angle) a)
            (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
    dispatch)
