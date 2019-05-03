#lang racket


(define-syntax (define-apl-fn stx)
  (syntax-case stx ()
    [(_
      (() e2 ...)
      ((e3) e4 ...)
      ((e5 e6) e7 ...))
     (with-syntax ([env (datum->syntax stx 'env)]
                   [args (datum->syntax stx 'args)])
       #'(lambda (env args)
           (match (length args)
             (0 (begin e2 ...))
             (1 (let ([e3 (car args)])
                  e4 ...))
             (2 (let ([e5 (car args)]
                      [e6 (cadr args)])
                  e7 ...))
             (else #f))))]))
