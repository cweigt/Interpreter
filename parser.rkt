#lang racket
(require "utils.rkt")
;a -> 1
;parser: a -> (var-exp a)
;executer: (var-exp a) -> go to var-scope and return 1

(define blaaade-parser
  (lambda (code)
    (cond
      ;this is definition of num-exp
      ((number? code) (list 'num-exp code))
      ;this is the definition of var-exp
      ((symbol? code) (list 'var-exp code))
      ;this is the definition of functin-exp
      ;(function (x) x) -> (func-exp (params x) (body-exp (var-exp x))
      ((null? code) (println "blaaade's parser is confused")) ;a grammar check
      ((eq? (car code) 'function)
       (list 'func-exp
             (list 'params (car (cadr code)))
             (list 'body-exp (blaaade-parser (caddr code)))))
      ;this is the definition of app-exp
      ;(call (function (x) x)) a) -> (app-exp (func-exp (params x) (body-exp (var-exp x)) (var-exp a))
                                    ;   <-1->     <-----------------2---------------->       <-3->
      ((eq? (car code) 'call)
       (list 'app-exp
             (blaaade-parser (cadr code))
             (blaaade-parser (caddr code))))
      ;this is definition of math-exp
      ;(1 + 2) --> (math-exp (num-exp 1) (op +) (num-exp 2))
      ((math-op? (cadr code))
       (list 'math-exp (blaaade-parser (car code))
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
      )))


(provide (all-defined-out))

