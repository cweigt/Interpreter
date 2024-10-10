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
    (cond
      ((null? scope) #f)
      ((eq? (car (car scope)) varname) #t)
      (else (scope-contains-name? varname (cdr scope)))
      )
    )
  )

(define env-contains-name?
  (lambda (varname env)
    (cond
      ((null? env) #f)
      ((scope-contains-name? varname (car env)) #t)
      (else (env-contains-name? varname (cdr env)))
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

;lst1 = (x y z) lst2 = (a b c)) -> ((x a) (y b) (z c))
(define combination
  (lambda (lst1 lst2)
    (cond
      ((null? lst1) '())
      (else (cons
             (list (car lst1) (car lst2))
             (combination (cdr lst1) (cdr lst2))))
      )
    )
  )

;env = (((c 4) (d 4)) ((a 1) (b 2) (x 5)))
;update d = 5
;results env = (((c 4) (d 4)) ((a 1) (b 5) (x 5)))
(define put-helper
  (lambda (varname value env)
    (cond
      ((or (null? env) (not (env-contains-name? varname env))) (displayln "variable is not in the env"))
      ((scope-contains-name? varname (car env))
       (cons (map (lambda (pair)
              (if (eq? (car pair) varname) (list varname value)
                  pair)) (car env)) (cdr env)))
      (else (cons (car env) (put-helper varname value (cdr env))))
     )
    )
  )

(provide (all-defined-out))