(eval '(+ 1 x) (extend-top-level-environment
                 user-initial-environment
                 '(x) '(3)))

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
        ((being? exp)
         (eval-sequence (begin-actions exp) env))
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

(x-then-y 1 2)
