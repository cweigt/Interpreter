#lang racket
(require "parser.rkt")
(require "utils.rkt")
(require "interpret.rkt")

(define var-env
  '(((a 1) (b 2) (x 5)))
  )

(define execute
  (lambda (code)
    (blaaade-interpreter (blaaade-parser code) var-env)
    )
  )

;(call (function (x) (
;queue
;(post c = 2)
;(c)
;) ((a)))

;(que-exp (out-exp (var-exp a)) (out-exp (var-exp b)))
;(blaaade-parser '(queue (out a) (out b)))
(execute '(queue (post c = 3) (out c)))
;post only makes sense in side queue-exp
;(let ((new_env (defined by post and current env))
;     (running the rest))
;post c = 3 is a statement to modify the environment