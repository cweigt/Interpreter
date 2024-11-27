#lang racket
(require "utils.rkt")

(define parser
  (lambda (code)
    (cond
      ;empty list is not a legit expression
      ((null? code) '())
      ;a number is a legit expression
      ((number? code) (list 'num-exp code))
      ;a symbol is a legit expression
      ((symbol? code) (list 'var-exp code))
      ((string? code) (list 'str-exp code))
      ;
      ((and (eq? (length code) 2) (symbol? (car code)))
       (cond
         ((eq? (car code) 'out) (list 'out-exp (parser (cadr code))))
         ((eq? (car code) 'return)
          (list 'return-exp (parser (cadr code))))
         (else
       (cons (parser (car code)) (parser (cdr code))))
       ))
      ;
      ((and (eq? (length code) 3) (or (symbol? (car code)) (math-op? (cadr code)) (bool-op? (cadr code))))
       (cond
         ((math-op? (cadr code))
          (list 'math-exp
             (parser (car code))
             (list 'op (cadr code))
             (parser (caddr code))))
         ((bool-op? (cadr code))
          (list 'bool-exp
             (parser (car code))
             (list 'op (cadr code))
             (parser (caddr code))))
         ((eq? (car code) 'wahl)
          (cons 'wahl-exp (map parser (cdr code))))
         ((eq? (car code) 'call)
          (list 'call-exp (parser (cadr code)) (map parser (caddr code))))
         (else
       (cons (parser (car code)) (parser (cdr code))))))
      ;
      ((and (eq? (length code) 4) (symbol? (car code)))
       (cond
         ((and (eq? (car code) 'post)
              (symbol? (cadr code))
            (eq? (caddr code) '=))
         (list 'post-exp (cadr code) (parser (cadddr code))))
         ((and (eq? (car code) 'put)
              (symbol? (cadr code))
            (eq? (caddr code) '=))
         (list 'put-exp (cadr code) (parser (cadddr code))))
         ((and (eq? (car code) 'ask))
          (cons 'ask-exp (map parser (cdr code))))
         ((eq? (car code) 'josh)
          (list 'josh-exp
                (parser (cadr code))
                (list 'func-exp
                      (map parser (caddr code))
                      (parser (cadddr code)))
                )
          )
         (else
       (cons (parser (car code)) (parser (cdr code))))))
      ((and (eq? (length code) 5) (symbol? (car code)))
       (cond
         ((eq? (car code) 'rough)
          (list
           'rough-exp
           (parser (cons 'post (cadr code)))
           (parser (caddr code))
           (parser (list 'put (car (cadr code)) '= (cadddr code)))
           (parser (get-list-item code 4)))
          )
         ((eq? (car code) 'mike)
          (cons 'mike-exp (cdr code)))
         ((eq? (car code) 'new)
          (list 'new-exp
               (parser (cadr code))
               (parser (cadddr code))
               (map parser (get-list-item code 4))
               )
          )
       (else
       (cons (parser (car code)) (parser (cdr code))))))
      ;anything else is just a list of expressions
      (else
       (cons (parser (car code)) (parser (cdr code))))
      )
    )
  )

(provide (all-defined-out))