#lang racket

(require "parser.rkt")
(require "utils.rkt")
(require "interpret.rkt")

(define var-scope
  '((a 1) (b 2) (x 5))
  )



(define execute
  (lambda (code)
    (blaaade-interpreter (blaaade-parser code) var-scope)
    )
  )


;(blaaade-interpreter (blaaade-parser '(call (function (x) x) y)) var-scope)
;(execute '(call (function (x) x) b))

;(blaaade-parser '(call (function (x) x) a))
;(execute '(call (function (x) x) a))

;(function-reverse (blaaade-parser '(call (function (x) x) 2)))

;(math-exp (num-exp 1) (op +) (num-exp 2))
;(execute '(call (function (x + 1) x) a))

(blaaade-parser '(call (function (x) (x * 2)) a))
(execute '(call (function (x) (x * 2)) a))