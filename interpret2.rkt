#lang racket
(require "utils.rkt")
(define interpreter
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code)
       (list null env)
       )
      ((eq? (car parsed-code) 'num-exp)
       (list (cadr parsed-code) env))
      ((eq? (car parsed-code) 'str-exp)
       (list (cadr parsed-code) env))
      ((eq? (car parsed-code) 'var-exp)
       (list (var-exp-helper parsed-code env) env))
      ((eq? (car parsed-code) 'out-exp)
       (screen-display
        (get-exp-return (cadr parsed-code) env))
       (list null env))
      ((eq? (car parsed-code) 'post-exp)
       (list null (post-helper
                   (cadr parsed-code)
                   (get-exp-return (caddr parsed-code) env)
                   env)))
      ((eq? (car parsed-code) 'put-exp)
       (list null
             (put-helper
              (cadr parsed-code)
                   (get-exp-return (caddr parsed-code) env)
                   env)))
      ((eq? (car parsed-code) 'math-exp)
       (list (math-exp-helper parsed-code env) env))
      ((eq? (car parsed-code) 'bool-exp)
       (list (boolean-exp-helper parsed-code env) env))
      ((eq? (car parsed-code) 'ask-exp)
       (if (get-exp-return (cadr parsed-code) env)
           (interpreter (caddr parsed-code) env)
           (interpreter (cadddr parsed-code) env)))
      ((eq? (car parsed-code) 'wahl-exp)
       (if (get-exp-return (cadr parsed-code) env)
           (interpreter parsed-code (get-exp-env (caddr parsed-code) env))
           (list null env)))
      ((eq? (car parsed-code) 'rough-exp)
       (let
           ((new-env (get-exp-env (cadr parsed-code) env))
            (new-parsed-code
             (list
              'ask-exp
              (caddr parsed-code)
              (list
               (get-list-item parsed-code 4)
               (cadddr parsed-code)
               (cons 'rough-nexp (cddr parsed-code)))
              '()))
            )
         (interpreter new-parsed-code new-env))) ;we convert the for-loop to be an if-statement
      ((eq? (car parsed-code) 'rough-nexp)
       (let
           ((new-parsed-code
             (list
              'ask-exp
              (cadr parsed-code)
              (list
               (cadddr parsed-code)
               (caddr parsed-code)
               parsed-code)
              '())))
         (interpreter new-parsed-code env)))
      ((eq? (car parsed-code) 'josh-exp)
       (list
        null
        (insert-helper
         (cadr (cadr parsed-code))
         (caddr parsed-code)
         env)))
      ((eq? (car parsed-code) 'call-exp)
       (let*
           ((func-code
             (resolve-env (cadr (cadr parsed-code)) env))
            (func-call (append func-code (list (caddr parsed-code))))
            )
         (interpreter func-call env))
       )
      ;(func-exp ((var-exp x)) (exp....)) (num-exp 5))
      ((eq? (car parsed-code ) 'func-exp)
       (let*
           ;lst1 = (x y z) lst2 = (a b c)) -> ((x a) (y b) (z c))
           ;combine
           ((new-scope
            (combination
             (map cadr (cadr parsed-code))
             (map (lambda (item) (get-exp-return item env)) (cadddr parsed-code))))
            (new-env (cons new-scope env))
            (exp-result (interpreter (caddr parsed-code) new-env))
            )
         (list
          (car exp-result)
          (cdr (cadr exp-result)))
         ))
      ((eq? (car parsed-code) 'mike-exp)
       (let*
           ((new-pair (list (cadr parsed-code) (cddr parsed-code)))
            (new-env (cons (cons new-pair (car env)) (cdr env))))
         (list null new-env)
       ))
      (else
       (if (eq? (car parsed-code) 'return-exp)
           (interpreter (cadr parsed-code) env)
           (interpreter (cdr parsed-code)
                         (get-exp-env (car parsed-code) env))))
      )
    )
  )

(define get-exp-return
  (lambda (parsed-exp env)
    (car (interpreter parsed-exp env))
    )
  )

(define get-exp-env
  (lambda (parsed-exp env)
    (cadr (interpreter parsed-exp env))
    )
  )

(define math-exp-helper
  (lambda (parsed-code env)
    (math-helper (cadr (caddr parsed-code))
                 (get-exp-return (cadr parsed-code) env)
                 (get-exp-return (cadddr parsed-code) env)
                 )
    )
  )

(define boolean-exp-helper
  (lambda (parsed-code env)
    (boolean-helper (cadr (caddr parsed-code))
                 (get-exp-return (cadr parsed-code) env)
                 (get-exp-return (cadddr parsed-code) env)
                 )
    )
  )

(provide (all-defined-out))