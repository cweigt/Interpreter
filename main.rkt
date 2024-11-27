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

;scope
;(department = "cs", subject = "language theory", lesson1 = {deparment: deparment...
;lesson: lesson1

(define code
  '(
    (mike lesson
         (department subject)
         (
          (put (this <- department) = deparment)
          (put (this <- subject) = subject)
          )
         (
          (talk () (out "we are talk about subject"))
          (displaySubject () (out subject))
          )
         )
    (new plt = lesson ("CS" "Language Theory"))
    )
  )

(parser code)
;(execute code)
;(null, (((lesson (source code)) rest-of-env)

;(new plt = lesson ("CS" "Language Theory"))
;(new-exp (var-exp plt) (var-exp lesson) ((str-exp "CS") (str-exp "Language Theory")