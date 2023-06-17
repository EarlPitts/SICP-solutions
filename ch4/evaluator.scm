;; (eval '(+ 1 x) (extend-top-level-environment
;;                  user-initial-environment
;;                  '(x) '(3)))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp) ; Literals
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((and? exp) (eval-and (operands exp) env))
        ((or? exp) (eval-or (operands exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error "Unknown expression type: EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend environment
                   (procedure-parameters procedure)
                   arguments
                   (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type: APPLY" procedure))))

; This is basically the same as mapping eval
(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent exp) env)
    (eval (if-alternative exp) env)))

(define (eval-and exps env)
  (cond ((bool-last-exp? exps)
         (eval (bool-first-exp exps) env))
        ((true? (eval (bool-first-exp exps) env))
         (eval-and (bool-rest-exps exps) env))
        (else false)))

(define (eval-or exps env)
  (cond ((bool-last-exp? exps)
         (eval (bool-first-exp exps) env))
        ((false? (eval (bool-first-exp exps) env))
         (eval-or (bool-rest-exps exps) env))
        (else true)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

; Exercise 4.1
; The only way to force execution order is by nesting lambdas (and begin)
; Force execution from left to right
(define (list-of-values2 exps env)
  (if (no-operands? exps)
    '()
    (((lambda first (lambda rest (cons first rest)))
     (list-of-values (rest-operands exps) env))
     (eval (first-operand exps) env))))
    
; Force execution from right to left
(define (list-of-values3 exps env)
  (if (no-operands? exps)
    '()
    (((lambda rest (lambda first (cons first rest)))
     (eval (first-operand exps) env))
     (list-of-values (rest-operands exps) env))))

(define (x-then-y x y)
  (((lambda first (lambda rest (cons first rest)))
    (begin
      (display y)
      y))
   (begin
     (display x)
     x)))

;; (x-then-y 1 2)

(define true #t)
(define false #f)
(define (true? b)
  (eq? b true))
(define (false? b)
  (eq? b false))

; Syntax
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((boolean? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)    ;formal parameters
                 (cddr exp))))  ;body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (bool-last-exp? exps) (null? (cdr exps)))
(define (bool-first-exp exps) (car exps))
(define (bool-rest-exps exps) (cdr exps))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last: COND->IF"
                 clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))

; Exercise 4.2
(define (call-application? exp) (tagged-list? exp 'call))
(define (call-operator exp) (cadr exp))
(define (call-operands exp) (cddr exp))
