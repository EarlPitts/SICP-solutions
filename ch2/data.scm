;Rational Numbers
;number: Selector for numerator
;denom: Selector for denominator
;make-rat: Constructor
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) 
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) 
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;Pairs
(define x (cons 1 2))

(car x)
(cdr x)
(cons 3 (cons 2 (cons 1 '())))

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(cdr (car z))

;Constructor and selector
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

;Printing
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

;Reducing
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(gcd 100 95)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(print-rat (add-rat one-third one-third))

;Exercise 2.1
(define (make-rat n d) 
  (let ((g ((if (< d 0) - +) (abs (gcd n d)))))
    (cons (/ n g) (/ d g)))) 
  
(print-rat (make-rat (- 1) 10))
(print-rat (make-rat 1 (- 10)))

;Exercise 2.2
(define (make-segment start end)
  (cons start end))

(define (start-segment line)
  (car line))

(define (end-segment line)
  (cdr line))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define a (make-point 5 9))
(define b (make-point 17 23))
(x-point a)
(y-point b)

(define line (make-segment a b))
(start-segment line)
(end-segment line)

(define (midpoint-segment line)
  (define (average a b) (/ (+ a b) 2))
  (let ((a (start-segment line))
        (b (end-segment line)))
    (make-point
      (average (x-point a) (x-point b))
      (average (y-point a) (y-point b)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(print-point (midpoint-segment line))

;Exercise 2.3
;Construct by corners
(define (make-rec a b)
  (cons a b))

(define (a-corner rec)
  (car rec))

(define (b-corner rec)
  (cdr rec))

(define (perimeter rec)
  (let ((a (a-corner rec))
        (b (b-corner rec)))
    (+ (* 2 (abs (- (x-point a) (x-point b))))
       (* 2 (abs (- (y-point a) (y-point b)))))))

(define (area rec)
  (let ((a (a-corner rec))
        (b (b-corner rec)))
    (* (abs (- (x-point a) (x-point b)))
       (abs (- (y-point a) (y-point b))))))

(define a (make-point 5 9))
(define b (make-point 17 23))
(perimeter (make-rec a b))
(area (make-rec a b))
(perimeter (make-rec (make-segment a b)))
(area (make-rec (make-segment a b)))

;Construct by diagonal
(define (make-rec line) line)

(define (a-corner rec)
  (start-segment rec))

(define (b-corner rec)
  (end-segment rec))

;Representing data with procedures
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))

(car (cdr (cons 3 (cons 1 2))))

;Exercise 2.4
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(car (cons 1 2))
(cdr (cons 1 2))

;Exercise 2.5
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

;TODO

;Exercise 2.6
;Church Numerals
;Each function call represents a natural number
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one 
  (lambda (f) (lambda (x) (f x))))

(define two 
  (lambda (f) (lambda (x) (f (f x)))))

(add-1 zero)
((zero -) 4)
;TODO

;Exercise 2.7
;Intervals
(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (let ((a (car interval))
        (b (cdr interval)))
    (min a b)))

(define (upper-bound interval)
  (let ((a (car interval))
        (b (cdr interval)))
    (max a b)))

(lower-bound (make-interval 8 7))
(upper-bound (make-interval 12 7))

;Exercise 2.8
(define (sub-interval i1 i2)
  (make-interval
    (- (lower-bound i1) (upper-bound i2))
    (- (upper-bound i1) (lower-bound i2))))

(define a (make-interval 8 7))
(define b (make-interval 12 7))
(define c (make-interval 20 4))
(sub-interval a b)

;Exercise 2.9
(define (width-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0)) 

(define (add-interval i1 i2)
  (make-interval 
    (+ (lower-bound i1) (lower-bound i2))
    (+ (upper-bound i1) (upper-bound i2))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y)))))


(define (compare l1 l2 f)
  (=
    (+ (width-interval l1) (width-interval l2))
    (width-interval (f l1 l2))))

(compare a b add-interval)
(compare a b sub-interval)
(compare a b mul-interval)
(compare a b div-interval)

;Exercise 2.10
(define (div-interval x y)
  (if (= (width-interval y) 0)
    (display "Error, division by zero!")
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

(div-interval a (make-interval 7 7))

;Exercise 2.11
(define (mul-interval x y)
  (cond (and (positive? (lower-bound x)) (positive? (upper-bound x)) (positive? (lower-bound y)) (positive? (upper-bound y))) (make-interval (* (lower-bound x) (lower-bound y) (* (upper-bound x)) (* (upper-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(center (make-center-width 3.5 0.15))
(width (make-center-width 3.5 0.15))

;Exercise 2.12
(define (percent i)
  (let ((c (center i)))
    (/ 100.0 (/ c (- c (lower-bound i))))))

(percent (make-center-percent 10 12))
(percent (make-interval 56 156))

(define (make-center-percent c p)
  (let ((err (* c (/ p 100.0))))
    (make-interval (- c err) (+ c err))))

(make-center-percent 10 5)

;Exercise 2.13
(define (product-percent i1 i2)
  (+ (percent i1) (percent i2)))

;Exercise 2.14
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval 
      one 
      (add-interval (div-interval one r1)
                    (div-interval one r2)))))

(par1 a b)
(par2 a b)

(define (div-interval x y)
  (if (= (width-interval y) 0)
    (display "Error, division by zero!")
    (mul-interval
      x
      (make-interval (/ 1.0 (upper-bound y))
                     (/ 1.0 (lower-bound y))))))

(div-interval one (add-interval (div-interval one a) (div-interval one b)))
(div-interval (mul-interval a b) (add-interval a b))
(define a (make-interval 6 7))
(define b (make-interval 7 8))
(define a (make-center-percent 6 5))
(define b (make-center-percent 7 5))
(percent (div-interval a a))
(percent b)
(div-interval a a)

(define one (make-interval 1 1))

(mul-interval a one)
(div-interval one a)

;Hierarchival Data Structures
;Sequence (list)
(cons 1 (cons 2 (cons 3 (cons 4 '()))))
;Syntactic Sugar
(list 1 2 3 4)

(cadddr (list 1 2 3 4 5))

;Get nth element from list
(list-ref (list 1 2 3 4) 3)

(define (get-n l i)
  (if (= i 0)
    (car l)
    (get-n (cdr l) (- i 1))))


(get-n (list 1 2 3 4) 3)

(define (get-n l i)
  (cond ((null? l) (display "Error, index out of bound!"))
        ((= i 0) (car l))
        (else (get-n (cdr l) (- i 1)))))

(get-n (list 1 2 3 4) 4)

(define (length l)
  (if (null? l)
    0
    (+ 1 (length (cdr l)))))

(length (list 1 2 3 4 5))

(define (length l)
  (define (iter l c)
    (if (null? l)
      c
      (iter (cdr l) (+ c 1))))
  (iter l 0))

;Append
(define (append l1 l2)
  (if (null? l1)
    l2
    (cons (car l1) (append (cdr l1) l2))))

(append (list 1 2 3) (list 4 5 6))

;Exercise 2.17
(define (last-pair l)
  (list-ref l (- (length l) 1)))

(define (last-pair l)
  (if (null? (cdr l))
    (car l)
    (last-pair (cdr l))))

(last-pair (list 1 2 3 4 5))

;Exercise 2.18
(define (reverse l)
  (if (null? l)
    l
    (cons (reverse (cdr l)) (car l))))

(define (reverse l)
  (if (null? (cdr l))
    l
    (append
      (reverse (cdr l))
      (cons (car l) '()))))

(define (reverse l)
  (define (iter l rev)
    (if (null? l)
      rev
      (iter (cdr l) (cons (car l) rev))))
  (iter l '()))

(reverse (list 1 2 3 4))

;Exercise 2.19
(define no-more? null?)
(define except-first-denomination cdr)
(define first-denomination car)

;Exercise 2.20
(define (test . z) z)
(define test (lambda z z))

(test 1 2 3)

(define (same-parity x . n)
  (define (iter x l result)
      (cond ((null? l) result)
            ((even? (+ x (car l))) (iter x (cdr l) (cons (car l) result)))
            (else (iter x (cdr l) result))))
  (iter x n '()))

(same-parity 1 2 3 4 5)
(same-parity 0 2 3 4 5)

;Map
(define (scale-list l factor)
  (if (null? l)
    '()
    (cons (* factor (car l))
          (scale-list (cdr l)
                      factor))))

(scale-list (list 1 2 3 4) 3)

(define (map f l)
  (if (null? l)
    '()
    (cons (f (car l))
          (map f (cdr l)))))

(map 1+ (list 1 2 3 4))
(map (lambda (x) (* x x)) (list 1 2 3 4))

(define (scale-list l factor)
  (map (lambda (x) (* factor x)) l))

(scale-list (list 1 2 3 4 5) 3)

;Exercise 2.21
(define (square-list l)
  (if (null? l)
    '()
    (cons (square (car l))
          (square-list (cdr l)))))

(square-list (list 1 2 3 4 5))

(define (square-list l)
  (map square l))

(square-list (list 1 2 3 4 5))

;Exercise 2.23
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

(define (for-each f l)
  (if (null? l)
    #t
    (and (f (car l)) 
         (for-each f (cdr l)))))

;Hierarchical Structures
(cons (list 1 2) (list 3 4))

(define x (cons (list 1 2) (list 3 4)))
(length x)

(define (count-leaves t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else (+ (count-leaves (car t))
                 (count-leaves (cdr t))))))

(count-leaves (list 1 2 3))
(count-leaves (list x x))

;Exercise 2.24
(list 1 (list 2 (list 3 4)))
(list 1 2)
(cons 1 (cons 2 '()))
(cons 2 (list 3 4)))
(cons 2 (cons (list 3 4) '()))
(cons 2 (cons (cons 3 (cons 4 '())) '()))
(cons 1 (cons (cons 2 (cons (cons 3 (cons 4 '())) '())) '()))
(list 2 3)

;Exercise 2.25
(car (cdr (car (cdr (cdr (list 1 3 '(5 7) 9))))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;Exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

(cons (cons 1 (cons 2 (cons 3 '()))) (cons 4 (cons 5 (cons 6 '()))))

;Exercise 2.27
(define (deep-reverse l)
  (define (iter l rev)
    (if (null? l)
      rev
      (iter (cdr l) 
            (cons 
              (if (list? (car l))
                (deep-reverse (car l))
                (car l))
              rev))))
  (iter l '()))

(deep-reverse '((1 2) (3 4)))

;Exercise 2.28
(define (fringe l)
  (if (null? l)
    l
    (if (not (pair? (car l)))
      (cons (car l) (fringe (cdr l)))
      (append (fringe (car l)) (fringe (cdr l))))))


(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))

;Exercise 2.29
(define (make-mobile left right)
  (cons left right))

(define (make-branch len structure)
  (cons len structure))

;Selectors
;Mobile
(define (left-branch mob)
  (car mob))

(define (right-branch mob)
  (cadr mob))

;Branch
(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mob)
  (let ((l (left-branch mob))
        (r (right-branch mob)))
    (+ (if (number? (branch-structure l))
         (branch-structure l)
         (total-weight (branch-structure l)))
       (if (number? (branch-structure r))
         (branch-structure r)
         (total-weight (branch-structure r))))))

(define mobile2
  (make-mobile
    (make-branch 2 2)
    (make-branch 2 2)))

(define mobile4
  (make-mobile
    (make-branch 2 2)
    (make-branch 2 2)))

(define mobile 
  (make-mobile
    (make-branch 2 mobile4)
    (make-branch 2 mobile2)))

(total-weight mobile)
(torque mobile2)

(define (torque mob)
  (let ((l (left-branch mob))
        (r (right-branch mob)))
    (=
      (* (branch-length l)
         (if (number? (branch-structure l))
           (branch-structure l)
           (total-weight (branch-structure l))))
      (* (branch-length r)
         (if (number? (branch-structure r))
           (branch-structure r)
           (total-weight (branch-structure r)))))))


(* (total-weight (branch-structure (left-branch mobile)))
(branch-length (left-branch mobile)))
(torque mobile)
(total-weight mobile4)
(total-weight mobile2)

(define (balanced? mob)
    (if (number? mob)
      #t
  (let ((l (left-branch mob))
        (r (right-branch mob)))
      (and (torque mob)
           (balanced? (branch-structure l))
           (balanced? (branch-structure r))))))

(balanced? mobile)

;Mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (* factor tree))
        (else (cons
                (scale-tree (car tree) factor)
                (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree sub-tree factor)
           (* sub-tree factor)))
       tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(list 1 2 3 4)
(cons 1 (cons 2 (cons 3 (cons 4 '()))))
(append (list 1 2 3) (list 3 4 5))
(define (append l1 l2)
  (if (null? (cdr l1))
    (car l1)
    (cons (append (car l1) (cdr l2)) l2)))



(cons 3 (cons 1 '()))

(define nil '())
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* factor tree))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree sub-tree factor)
           (* sub-tree factor)))
       tree))


;Exercise 2.30
;Without map
(define (square-tree t)
  (cond ((null? t) nil)
        ((not (pair? t)) (square t))
        (else (cons (square-tree (car t))
                    (square-tree (cdr t))))))


;With map
(define (square-tree t)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       t))

(square-tree
  (list 1
        (list 2 (list 3 4) 5)
        (list 6 7)))

;Exercise 2.31
(define (tree-map f t)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (f sub-tree)))
       t))

(define (square-tree tree)
  (tree-map square tree))

;Exercise 2.32
(define (identity x) x)
(define nil '())

(define (subsets s)
  (if (null? s)
    (list nil)
    (let ((rest (subsets (cdr s))))
      (append rest (map
                     (lambda (e) 
                       (cons (car s) e))
                     rest)))))

(subsets (list 1 2 3))
(map (lambda (e) e) (list nil))
(list nil)

;Conventional Interfaces
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree)
           (square tree)
           0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(sum-odd-squares (list 1 (list 2 3) (list 2 3 4) 3 4 (list 1 (list 2 3) 2)))

(define (fib n)
  (define (iter a b resp)
    (if (> a n)
      resp
      (iter (+ a b) a (cons b resp))))
  (iter 1 1 '()))

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib n)
  (define (iter a b)
    (if (> b n)
      b
      (iter (+ a b) a)))
  (iter 1 1))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
      '()
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))

(define (range n)
  (if (= n 0)
    '()
    (cons n (range (- n 1)))))

(define (range n)
  (define (iter n l)
    (if (> n 0)
      (iter (- n 1) (cons n l))
      l))
  (iter n '()))

(range 10)

(define (fib-list n)
  (map fib (range n)))

(fib-list 20)

(even-fibs 20)

;Filter
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(filter odd? (range 10))

;Reduce
(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op 
      (car seq) 
      (accumulate op initial (cdr seq)))))

(accumulate + 0 (range 10))

;Enumerate
(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate
    + 0 (map square (filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
      '()
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))

(define (even-fibs n)
  (accumulate
    cons '() (filter even? (map fib (range n)))))

(sum-odd-squares (list 1 (list 2 3) (list 2 3 4) 3 4 (list 1 (list 2 3) 2)))

;Exercise 2.33
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
        (accumulate cons seq2 seq1))

(define (length seq)
        (accumulate (lambda (x y) (+ 1 y)) 0 seq))

;Exercise 2.34
(define (horner-eval x coefficient-seq)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-seq))

(horner-eval 2 (list 1 3 0 5 0 1))

;Exercise 2.35
(define (count-leaves t)
  (accumulate
    +
    0
    (map 
      (lambda (t)
        (if (pair? t)
          (count-leaves t)
          1))
      t)))

(count-leaves (list 1 2 3 (list 4 5) (list 6 7 (list 1 2) 2) 3))

;Exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n cons '() s)

;Exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(dot-product (list 1 2 3) (list 2 3 4))

(define (map op . ln)
  (cons (map op (car ln))
        (map op (cdr ln))))

(map + (list 1 2 3) (list 2 3 4))

;Exercise 2.37
(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(define v (list 1 2 3))

(define (matrix-*-vector m v)
  (map 
    (lambda (row) (dot-product row v))
      m))

(matrix-*-vector m v)

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose m)

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map 
      (lambda (row) 
        (map 
          (lambda (col)
            (dot-product row col))
          cols))
      m)))

(define m (list (list 1 2) (list 3 4)))
(matrix-*-matrix m m)

;Exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
  (iter initial sequence))

(accumulate / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(accumulate list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))

;Exercise 2.39
(define (reverse seq)
  (accumulate 
    (lambda (x y)
      (cons 
        (list-ref seq (- (length seq) x)) 
        y))
    '() 
    seq))

(define (reverse seq)
  (accumulate 
    (lambda (x y)
      (append y (list x)))
    '() 
    seq))

(define (reverse seq)
  (fold-left (lambda (x y) (cons y x)) '() seq))

(reverse (list 1 2 3 4))

;Nested Mappings

; (accumulate
;   append '() (map (lambda (x) (list (square x))) (list 1 2 3)))

(map (lambda (i)
    (map (lambda (j) (list i j))
            (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n))

(map (lambda (x) x) (enumerate-interval 1 n))

(define n 5)
(accumulate
  append '() (map (lambda (i)
                    (map (lambda (j) (list i j))
                         (enumerate-interval 1 (- i 1))))
                  (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                            (lambda (i)
                              (map (lambda (j) (list i j))
                                   (enumerate-interval 1 (- i 1))))
                            (enumerate-interval 1 n)))))

(prime-sum-pairs 5)

(define (permutations s)
  (if (null? s)
    (list '())
    (map (lambda (x)
               (map (lambda (p) (cons x p))
                    (permutations (remove x s))))
             s)))

(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))

(permutations (list 1 2 3 4))

;Exercise 2.40
(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? 
               (unique-pairs n))))

(prime-sum-pairs 3)

;Exercise 2.41
(define (equal-sum-triplets? s t)
  (= s (sum-triplets t)))

(define (sum-triplets t)
  (+ (car t) (cadr t) (caddr t)))

(define (make-triplet-sum t)
  (list (car t) (cadr t) (caddr t) (sum-triplets t)))


(define (unique-triplets n)
  (flatmap
    (lambda (i)
      (flatmap (lambda (j) 
             (map (lambda (k) (list i j k))
                  (enumerate-interval 1 (- j 1))))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(map (make-triplet-sum 
    (filter equal-sum-triplets? s
        (unique-triplets 5))))

;Exercise 2.42
(define empty-board '())

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                     new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define a (list (list 2 3 4) (list 1 2 3)))
(list (list 2 3 4) (list 1 2 3))
(flatmap (lambda (a) (list a 1)) (list (list 1 2 3)))
(flatmap (lambda (a) (list a)) (list (list 1 2) (list 2 3)))

;Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

;Exercise 2.45
(define (split fst snd)
  (lambda (painter n)
    (if (= n 0)
      painter
      (let ((smaller ((split fst snd) painter (- n 1))))
        (snd painter (fst smaller smaller))))))

; Frames
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge2-frame frame))))))

;Exercise 2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect
    (+ (xcor-vect v1) (xcor-vect v2))
    (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
    (- (xcor-vect v1) (xcor-vect v2))
    (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect
    (* s (xcor-vect v))
    (* s (ycor-vect v))))


(define o (make-vect 2 2))
(define a (make-vect 4 7))
(define b (make-vect 5 2))

(add-vect a b)
(sub-vect a b)
(scale-vect 2 b)
(xcor-vect b)
(ycor-vect b)

;Exercise 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

(define (edge2-frame frame)
  (cddr frame))

(origin-frame (make-frame o a b))
(edge1-frame (make-frame o a b))
(edge2-frame (make-frame o a b))

(make-frame o a b)
(define f (make-frame o b a))

(define m (frame-coord-map f))

(m (make-vect 0 1))

(let ((new-origin (m (make-vect 0 1))))
  (make-frame 
    new-origin
    (sub-vect (m (make-vect 1 1)) new-origin)
    (sub-vect (m (make-vect 0 0)) new-origin)))


((frame-coord-map (make-frame (make-vect 2 2) a b))
 (make-vect 0 0))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line
          ((frame-coord-map frame)
           (start-segment segment))
          ((frame-coord-map frame)
           (end-segment segment))))
      segment-list)))

;Exercise 2.48
(define (make-segment start end)
  (cons start end))

(define (start-segment seg)
  (car seg))

(define (end-segment seg)
  (cdr seg))

;Exercise 2.49
(list (make-segment
        (make-vect 0 0)
        (make-vect 1 0))
      (make-segment
        (make-vect 1 0)
        (make-vect 1 1))
      (make-segment
        (make-vect 1 1)
        (make-vect 1 0))
      (make-segment
        (make-vect 1 0)
        (make-vect 0 0)))

(list (make-segment
        (make-vect 0 0)
        (make-vect 1 1))
      (make-segment
        (make-vect 1 0)
        (make-vect 0 1)))

(list (make-segment
        (make-vect 0.5 0)
        (make-vect 1 0.5))
      (make-segment
        (make-vect 1 0.5)
        (make-vect 0.5 1))
      (make-segment
        (make-vect 0.5 1)
        (make-vect 0 0.5))
      (make-segment
        (make-vect 0 0.5)
        (make-vect 0.5 0)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                   new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin)))))))


;Exercise 2.50
(define (flip-horiz painter)
  (transform-painter
    painter
    (make-vect 1.0 0.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))

(define (rotate-180 painter)
  (transform-painter
    painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)))

(define (rotate-270 painter)
  (transform-painter
    painter
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))

;Exercise 2.51
(define (below p1 p2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
            (transform-painter
              (make-vect 0.0 0.0)
              (make-vect 1.0 0.0)
              split-point))
          ((paint-top
             (transform-painter
              split-point
              (make-vect 1.0 0.5)
              (make-vect 0.0 1.0)))))
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))

(define (below p1 p2)
  (rotate-90 (beside 
               (rotate-270 p1)
               (rotate-270 p2))))


;Symbolic Data
(list (list 'list 1 2 3) 1)
(list 'car (list 'quote '(a b c)))

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(memq 'apple (list 1 'apple 2 3))

;Exercise 2.53
(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

(eq? '(list 1 2 3) '(list 1 2 3))

;Exercise 2.54
(define (equal? s1 s2)
  (if (not (and (pair? s1) (pair? s2)))
    (eq? s1 s2)
    (and (equal? (car s1)
                 (car s2))
         (equal? (cdr s1)
                 (cdr s2)))))

;Exercise 2.55
(car ''abracadabra)
(car '(quote abracadabra))

;Differentiation System
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product
           (make-product (exponent exp)
                         (make-exponentiation (base exp)
                                              (- (exponent exp) 1)))
           (deriv (base exp) var)))
        (else
          (error "Unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base e) (cadr e))
(define (exponent e) (caddr e))

(deriv '(** x 2) 'x)

;Exercise 2.57
;TODO
(define (make-product . mn) 
  (define (iter-product-list l product result)
    (cond ((and (null? l) (= product 1)) (car result))
          ((null? l) (cons '* (cons product result)))
          ((number? (car l)) (iter-product-list (cdr l) (* (car l) product) result))
          (else (iter-product-list (cdr l) product (cons (car l) result)))))
  (iter-product-list mn 1 '()))

(define (make-sum . an)
  (define (iter-sum-list l sum result)
    (cond ((and (null? l) (= sum 0)) (car result))
          ((null? l) (cons '+ (cons sum result)))
          ((number? (car l)) (iter-sum-list (cdr l) (+ (car l) sum) result))
          (else (iter-sum-list (cdr l) sum (cons (car l) result)))))
  (iter-sum-list an 0 '()))

;Exercise
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))
(define (addend s) (car s))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))
(define (multiplier p) (car p))

;Sets
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

;Exercise 2.59
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else 
          (cons (car s1) (union-set (cdr s1) s2)))))

;Exercise 2.60
(define (adjoin-set x set)
  (cons x set))

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) '())
        ((element-of-set? (car s1) s2)
         (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

(define (union-set s1 s2)
  (append s1 s2))

;Set as Ordered List
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set s1 s2)
  (if (or (null? s1) (null? s2))
    '()
    (let ((x1 (car s1)) (x2 (car s2)))
      (cond ((= x1 x2)
             (cons x1 (instersection-set (cdr s1)
                                         (cdr s2))))
            ((< x1 x2)
             (intersection-set (cdr s1) s2))
            ((> x1 x2)
             (intersection-set s1 (cdr s2)))))))
          
;Exercise 2.61
(define (adjoin-set x set)
  (cond ((null? set) (list x))
    ((> x (car set))
        (cons (car set) (adjoin-set x (cdr set))))
    (else (cons x set))))

;Exercise 2.62
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
          (let ((x1 (car s1)) (x2 (car s2)))
          (cond ((= x1 x2)
                 (cons x1 (union-set (cdr s1)
                                     (cdr s2))))
                ((< x1 x2)
                 (cons x1 (union-set (cdr s1) s2)))
                ((> x1 x2)
                 (cons x2 (union-set s1 (cdr s2)))))))))

;Sets as Binary Trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set) #t))
        ((< x (entry set)
            (element-of-set? x (left-branch set))))
        ((> x (entry set)
            (element-of-set? x (right-branch set))))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;Exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1
                    (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list
                            (right-branch tree)
                            result-list)))))
  (copy-to-list tree '()))

                    

(tree->list-1 (adjoin-set 2 (adjoin-set 8 (adjoin-set 4 (adjoin-set 3 '())))))

(tree->list-2 (adjoin-set 2 (adjoin-set 8 (adjoin-set 4 (adjoin-set 3 '())))))

;Exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result
              (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                 (right-result
                   (partial-tree
                     (cdr non-left-elts)
                     right-size)))
                 (let ((right-tree (car right-result))
                       (remaining-elts
                         (cdr right-result)))
                   (cons (make-tree this-entry
                                    left-tree
                                    right-tree)
                         remaining-elts)))))))))

; TODO
(list->tree (list 1 2 3 4))
(partial-tree (list 1 2 3 4) 4)

;Exercise 2.65
; TODO

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (lookup given-key tree)
  (cond ((null? tree) false)
        ((equal? given-key (key (entry tree)))
         (entry tree))
        ((< given-key (key (entry tree)))
         (lookup given-key (left-branch tree)))
        ((> given-key (key (entry tree)))
         (lookup given-key (right-branch tree)))))

(define (key node) node)

;Huffman Encoding Trees
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (symbol-weight x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
    (symbol-weight tree)
    (cadddr tree)))

;Decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch
              (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch)
                (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

;Generating
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)     ;symbol
                             (cadr pair))   ;frequency
                  (make-leaf-set (cdr pairs))))))


;Exercise 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

(decode sample-message sample-tree)

;Exercise 2.68
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (element? symbol l)
  (cond ((null? l) #f)
        ((eq? symbol (car l)) #t)
        (else (element? symbol (cdr l)))))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "bad symbol"))))

(equal?
  sample-message
  (encode (decode sample-message sample-tree) sample-tree))

;Exercise 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define pairs '((A 1) (B 2) (C 1)))
(make-leaf-set pairs)

(define (successive-merge set)
  (if (= (length set) 1)
    (car set)
    (successive-merge
      (adjoin-set
        (make-code-tree (car set) (cadr set))
        (cddr set)))))

;Exercise 2.70
(define pairs '((A 2) (GET 2) (SHA 3) (WAH 1)
                (BOOM 1) (JOB 2) (NA 16) (YIP 9)))

(define tree (generate-huffman-tree pairs))
(display tree)

(define message '(Get a job sha na na na na na na na na
                  Get a job sha na na na na na na na na
                  Wah yip yip yip yip yip yip yip yip yip
                  Sha boom))

(length (encode message tree))
