#lang racket
(require "utils.rkt")

;(define var-scope '((x a) (a 1) (b 2) (x 5)))
(define blaaade-interpreter
  (lambda (parsed-code scope)
    (cond
      ((null? parsed-code) (println "blaaade's interpreter found illegal code."))
      ((eq? (car parsed-code) 'var-exp) (var-exp-helper parsed-code scope))
      ((eq? (car parsed-code) 'num-exp) (cadr parsed-code))
      ((eq? (car parsed-code) 'math-exp) (math-exp-helper parsed-code scope))
      ((eq? (car parsed-code) 'app-exp)
       (let
           (
            (local_scope
             (cons (list (cadr (cadr (cadr parsed-code))) (blaaade-interpreter (caddr parsed-code) scope)) scope))
            )
         (blaaade-interpreter (cadr (caddr (cadr parsed-code))) local_scope)
         )
       )
      (else (println "blaaade's interpreter found illegal code."))
      )
    )
  )

  ;(var-exp a) resolve the value from the variable expression based on scope
  ;(1 + 2) --> (math-exp (num-exp 1) (op +) (num-exp 2))
(define var-exp-helper
  (lambda (parsed-code scope)
    (resolve-helper (cadr parsed-code) scope)
    )
  )

(define math-exp-helper
  (lambda (parsed-code scope)
    (math-helper (cadr (caddr parsed-code))
                 (blaaade-interpreter (cadr parsed-code) scope)
                 (blaaade-interpreter (cadddr parsed-code) scope)
                 )
    )
  )

(define math-helper
  (lambda (op num1 num2)
    (cond
      ((eq? op '+) (+ num1 num2))
      ((eq? op '-) (- num1 num2))
      ((eq? op '*) (* num1 num2))
      ((eq? op '/) (/ num1 num2))
      ((eq? op '%) (modulo num1 num2))
      (else (println "blaaade do not know this operation"))
      )
    )
  )

(provide (all-defined-out))