; Get the bigger number
(define (bigger x y) (if (> x y) x y))

(define (smaller x y) (if (< x y) x y))

(bigger 1 2)

(define (square x) (* x x))

(square 4)

(define (biggest x y z)
  (bigger (bigger x y) z))

(define (smallest x y z)
  (smaller (smaller x y) z))

(define (biggest-two x y z)
  (cond
    ((= (smallest x y z) x) (list y z))
    ((= (smallest x y z) y) (list x z))
    ((= (smallest x y z) z) (list x y))))

(biggest-two 11 20 30)

(define (solution x y z)
  (map + (map square (biggest-two x y z))))

(solution 39 2 3)
