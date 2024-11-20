#lang racket
(require "utils.rkt")
(require "parser2.rkt")
(require "interpret2.rkt")

(define var-env
  '(((a 1) (b 2) (x 5)))
  )

(define execute
  (lambda (code)
    (interpreter (parser code) var-env)
    )
  )

;for i = 0; i < 10; i++
(define code '(josh square (x) (out (x * x))))
;josh-exp (var-exp square) (func-exp ((var-exp x) (var-exp y) (var-exp z)) (parsed-code))
;we will create a new key-value pair in the top-most variable scope
;key = square //function name
;value = (func-exp ((var-exp x) (var-exp y) (var-exp z)) (parsed-code) //function expression
;(parser code)
;(execute code)

(define code1 '(
                (josh square (x) (return (x * x)))
                (put a = (call square (5)))
                (out a)
                )
  
  )


;(parser code1)
;(... (return-exp (var-exp 5)))....
(execute code1)
;make a return expression