#lang racket
(require "utils.rkt")

;(define var-env '(((x a) (a 1) (b 2) (x 5))))
(define blaaade-interpreter
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) (println "blaaade's interpreter found illegal code."))
      ((eq? (car parsed-code) 'var-exp) (var-exp-helper parsed-code env))
      ((eq? (car parsed-code) 'num-exp) (cadr parsed-code))
      ((eq? (car parsed-code) 'app-exp)
       (let
           (
            (local_env
             (cons
              (list (list (cadr (cadr (cadr parsed-code))) (blaaade-interpreter (caddr parsed-code) env)))
              env))
            )
         (blaaade-interpreter (cadr (caddr (cadr parsed-code))) local_env)
         )
       )
      ((eq? (car parsed-code) 'math-exp) (math-exp-helper parsed-code env))
      ((eq? (car parsed-code) 'boolean-exp) (boolean-exp-helper parsed-code env))
      (else (println "blaaade's interpreter found illegal code."))
      )
    )
  )

  ;(var-exp a) resolve the value from the variable expression based on env
(define var-exp-helper
  (lambda (parsed-code env)
    (resolve-env (cadr parsed-code) env)
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
;(math-exp (num-exp 1) (op +) (num-exp 2))
(define math-exp-helper
  (lambda (parsed-code env)
    (math-helper (cadr (caddr parsed-code))
                 (blaaade-interpreter (cadr parsed-code) env)
                 (blaaade-interpreter (cadddr parsed-code) env)
                 )
    )
  )

(define boolean-helper
  (lambda (op num1 num2)
    (cond
      ((eq? op '>) (> num1 num2))
      ((eq? op '<) (< num1 num2))
      ((eq? op '>=) (>= num1 num2))
      ((eq? op '<=) (<= num1 num2))
      ((eq? op '==) (eq? num1 num2))
      ((eq? op '!=) (not (eq? num1 num2)))
      (else (println "blaaade do not know this operation"))
      )
    )
  )
;(math-exp (num-exp 1) (op +) (num-exp 2))
(define boolean-exp-helper
  (lambda (parsed-code env)
    (boolean-helper (cadr (caddr parsed-code))
                 (blaaade-interpreter (cadr parsed-code) env)
                 (blaaade-interpreter (cadddr parsed-code) env)
                 )
    )
  )

(provide (all-defined-out))