#lang racket

(define resolve-helper
  (lambda (varname scope)
    (cond
      ((null? scope) (println "blaaade cannot find the variable"))
      ((eq? (car (car scope)) varname) (cadr (car scope)))
      (else (resolve-helper varname (cdr scope)))
      )
    )
  )

(define function-reverse
  (lambda (parsed-code)
    (cond
      ((null? parsed-code) '())
      ((equal? (car parsed-code) 'func-exp)
       (list 'function ;starting list with the word function
             (list (cadr (cadr parsed-code))) ;get params *x, stays same
                (cadr (cadr (caddr parsed-code))))) ;second element of list 'function, this is x in the body-exp
      ((or (equal? (car parsed-code) 'var-exp) (equal? (car parsed-code) 'num-exp)) (cadr parsed-code)) ;should get a/b/c or whatever
      ((equal? (car parsed-code) 'app-exp)
       (list 'call
            (function-reverse (cadr parsed-code)) ;calls on func
            (function-reverse (caddr parsed-code)))) ;calls on var-exp a
      (else parsed-code))))

(provide (all-defined-out))