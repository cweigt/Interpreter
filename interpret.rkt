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
      ;'(wahl-exp (boolean-exp (var-exp a) (op <) (num-exp 10)) (body-exp (queue-exp (put-exp (var-exp a) (math-exp (var-exp a) (op +) (num-exp 1))) (out-exp (var-exp a)))))
      ((eq? (car parsed-code) 'wahl-exp)
       (if (blaaade-interpreter (cadr parsed-code) env)
           ;this is what I'm getting on display
           ;(queue-exp (queue-exp (put-exp (var-exp a) (math-exp (var-exp a) (op +) (num-exp 1))) (out-exp (var-exp a))))
             (displayln
              (cons 'queue-exp (list (cadr (caddr parsed-code)))) )
 
           (println "Condition false: all done")))

      ((eq? (car parsed-code) 'out-exp)
       (displayln (blaaade-interpreter (cadr parsed-code) env)))
      ((eq? (car parsed-code) 'queue-exp)
       ;(queue (post c = 3) (post d = 4)) if next exp is post only, do nothing
       ;otherwise, execute the next next exp in the new scope
       (cond
         ((and (eq? (length parsed-code) 2) (eq? (car (cadr parsed-code)) 'post-exp))
          (println "declare without use"))
         ((and (eq? (length parsed-code) 2) (eq? (car (cadr parsed-code)) 'put-exp))
          (println "assign without use"))
         ((and (eq? (length parsed-code) 2)
               (blaaade-interpreter (cadr parsed-code) env)))
         ((eq? (car (cadr parsed-code)) 'post-exp)
          ;case 1: the variable name is in the env, throw error
          (if (env-contains-name? (cadr (cadr parsed-code)) env)
           (println "Cannot declare used variable.")
          ;case 2: add the kvp to the top scope of the env, run the rest of queue
           (let ((new-env
                  ;post-exp (var-exp c)(num-exp 3)
                  (cons (cons (list (cadr (cadr (cadr parsed-code))) (blaaade-interpreter (caddr (cadr parsed-code)) env)) (car env)) (cdr env))))
             (blaaade-interpreter (cons 'queue-exp (cddr parsed-code)) new-env))))
         ((eq? (car (cadr parsed-code)) 'put-exp)
          (if (env-contains-name? (cadr (cadr (cadr parsed-code))) env)
              (let ((new-env
                     (put-helper (cadr (cadr (cadr parsed-code))) (blaaade-interpreter (caddr (cadr parsed-code)) env) env)))
                (blaaade-interpreter (cons 'queue-exp (cddr parsed-code)) new-env))
              (displayln "variable is not in the env")))    
         (else
          (blaaade-interpreter (cadr parsed-code) env)
          (blaaade-interpreter (cons 'queue-exp (cddr parsed-code)) env))))
      ((eq? (car parsed-code) 'math-exp) (math-exp-helper parsed-code env))
      ;'(ask-exp (boolean-exp (var-exp a) (op ==) (num-exp 1))
      ;(true-exp (var-exp b)) (false-exp (var-exp x)))
      ((eq? (car parsed-code) 'ask-exp)
       (if (blaaade-interpreter (cadr parsed-code) env)
           (blaaade-interpreter (cadr (caddr parsed-code)) env)
           (blaaade-interpreter (cadr (cadddr parsed-code)) env)
           ))
      
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