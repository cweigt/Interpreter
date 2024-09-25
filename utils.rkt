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

(define resolve-scope
  (lambda (varname scope)
    (cond
      ((null? scope) (println "blaaade cannot find the variable"))
      ((eq? (car (car scope)) varname) (cadr (car scope)))
      (else (resolve-scope varname (cdr scope)))
      )
    )
  )

;return true if the scope has the variable name, otherwise return false
(define scope-contains-name?
  (lambda (varname scope)
    (
     cond
      ((null? scope) #f)
      ((eq? (car (car scope)) varname) #t)
      (else (scope-contains-name? varname (cdr scope)))
      )
    )
  )

(define resolve-env
  (lambda (varname env)
    (cond
      ((null? env) (println "no such variable in the environment."))
      ((scope-contains-name? varname (car env)) (resolve-scope varname (car env)))
      (else (resolve-env varname (cdr env)))
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