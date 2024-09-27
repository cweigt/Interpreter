#lang racket
(require "utils.rkt")

;(define var-env '(((x a) (a 1) (b 2) (x 5))))
(define blaaade-interpreter
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) (println "blaaade's interpreter found illegal code."))
      ((eq? (car parsed-code) 'var-exp) (var-exp-helper parsed-code env))
      ((eq? (car parsed-code) 'num-exp) (cadr parsed-code))
      ;(app-exp (func-exp ((parmas x) (params y)) (body-exp (math-exp
      ;(var-exp x) (op +) (var-exp y)) ((var-exp a) (var-exp b)))
      ((and (eq? (car parsed-code) 'app-exp)
            (eq? (length (cadr (cadr parsed-code)))
            (length (caddr parsed-code))))
       (let
           (
            (local_env
             (cons
              (combination
               (map cadr (cadr (cadr parsed-code)))
               (map (lambda (x) (blaaade-interpreter x env)) (caddr parsed-code)))
              env))
            )
         (blaaade-interpreter (cadr (caddr (cadr parsed-code))) local_env)
         )
       )
      ((eq? (car parsed-code) 'math-exp) (math-exp-helper parsed-code env))
      ;(ask-exp (boolean-exp (var-exp a) (op ==) (num-exp 1)) (true-exp (var-exp b)) (false-exp (var-exp x)))
      ((eq? (car parsed-code) 'ask-exp)
       (if (blaaade-interpreter (cadr parsed-code) env)
           (blaaade-interpreter (cadr (caddr parsed-code)) env)
           (blaaade-interpreter (cadr (cadddr parsed-code)) env)))
      ;(boolean-exp (op !) (var-exp a))
      ((and (eq? (car parsed-code) 'boolean-exp)
            (eq? (car (cadr parsed-code)) 'op))
       (not (blaaade-interpreter (caddr parsed-code) env)))
      ((and
        (eq? (car parsed-code) 'boolean-exp)
        (eq? (car (caddr parsed-code)) 'op))
        (boolean-exp-helper parsed-code env))
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
;(boolean-exp (num-exp 1) (op >) (num-exp 2))... ignore this dog water comment
(define boolean-exp-helper
  (lambda (parsed-code env)
    (boolean-helper (cadr (caddr parsed-code))
                 (blaaade-interpreter (cadr parsed-code) env)
                 (blaaade-interpreter (cadddr parsed-code) env)
                 )
    )
  )

(provide (all-defined-out))