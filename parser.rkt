#lang racket

;a -> 1
;parser: a -> (var-exp a)
;executer: (var-exp a) -> go to var-scope and return 1

(define blaaade-parser
  (lambda (code)
    (cond
      ;this is the definition of num-exp
      ((number? code) (list 'num-exp code))
      ;this is the definition of var-exp
      ((symbol? code) (list 'var-exp code))
      ;this is the definition of functin-exp
      ;(function (x y) x) -> (func-exp ((params x) (params y)) (body-exp (var-exp x))
      ((null? code) (println "blaaade's parser is confused")) ;a grammar check
      ((eq? (car code) 'function)
       (list 'func-exp
             (map (lambda (x) (list 'params x)) (cadr code))
             (list 'body-exp (blaaade-parser (caddr code)))))
      ;this is the definition of app-exp
      ;(call (function (x) x)) (a)) ->
      ;(app-exp (func-exp (params x) (body-exp (var-exp x)) ((var-exp a))
      ((eq? (car code) 'call)
       (list 'app-exp
             (blaaade-parser (cadr code))
             (map blaaade-parser (caddr code))))
      ;(ask (a == 1) b x) -> (ask-exp (boolean-exp (var-exp a) (op ==) (num-exp 1))
      ; (true-exp (var-exp b))
      ; (false-exp (var-exp x))
      ((eq? (car code) 'ask)
       (list 'ask-exp
             (blaaade-parser (cadr code))
             (list 'true-exp (blaaade-parser (caddr code)))
             (list 'false-exp (blaaade-parser (cadddr code))))
       )
      ;this is the definition of math-exp
      ;((1+1) + 2) -> (math-exp (num-exp 1) (op +) (num-exp 2))
      ((eq? (car code) '!)
       (list 'boolean-exp (list 'op '!) (blaaade-parser (cadr code))))
      ((math-op? (cadr code))
       (list 'math-exp (blaaade-parser (car code))
             (list 'op (cadr code))
             (blaaade-parser (caddr code))))
      ;;(x == 1) -> (boolean-exp (var-exp x) (op ==) (num-exp 1)
      ((bool-op? (cadr code))
       (list 'boolean-exp (blaaade-parser (car code))
             (list 'op (cadr code))
             (blaaade-parser (caddr code))))
      (else (println "blaaade's parser is confused"))
      )
    )
  )

(define math-op?
  (lambda (op)
    (cond
      ((eq? op '+) #t)
      ((eq? op '-) #t)
      ((eq? op '/) #t)
      ((eq? op '*) #t)
      ((eq? op '%) #t)
      (else #f)
      )
    )
  )

(define bool-op?
  (lambda (op)
    (cond
      ((eq? op '>) #t)
      ((eq? op '<) #t)
      ((eq? op '>=) #t)
      ((eq? op '<=) #t)
      ((eq? op '==) #t)
      ((eq? op '!=) #t)
      ((eq? op '&&) #t)
      ((eq? op '||) #t)
      ((eq? op '!) #t)
      (else #f)
      )
    )
  )
    
(provide (all-defined-out))