#lang racket

(require "syms.rkt")

(define (unicode-symbol? s)
  (member s unicode-symbols string=?))

(define (operator? s)
  (member s operator-symbols string=?))

(define (digit? s)
  (member s digit-symbols string=?))

(define (symbol-char? s)
  (member s word-symbols string=?))

(define (append-while lst validator ret-str)
  (cond
    ((empty? lst) (values ret-str lst))
    ((validator (car lst)) (append-while (cdr lst) validator (string-append ret-str (car lst))))
    (else (values ret-str lst))))

(define (read-digits lst)
  (append-while lst digit? ""))

(define (read-word lst)
  (append-while lst (λ (s) (or (symbol-char? s)
                               (digit? s))) ""))

(define expr-test "v1←2 + (+/ 1 2 3) * 11 2 3 (4+4) 2 + 3 6")

(define (convert s)
  (map (λ (x) (format "~a" x))
       (string->list s)))

(define (get-tokens lst #:token-list [token-list '()])
  (cond
    ((empty? lst) token-list)
    (else (let ([head (first lst)])
            (cond
              ((unicode-symbol? head)
               (get-tokens (cdr lst)
                           #:token-list (cons (cons 'unicode-operator
                                                    head)
                                              token-list)))
              ((operator? head)
               (get-tokens (cdr lst)
                           #:token-list (cons (cons 'operator
                                                    head)
                                              token-list)))
              ((digit? head)
               (let-values ([(digit rest-list) (read-digits lst)])
                 (get-tokens rest-list #:token-list (cons (cons 'numeric
                                                                (string->number digit))
                                                          token-list))))
              ((symbol-char? head)
               (let-values ([(word rest-list) (read-word lst)])
                 (get-tokens rest-list #:token-list (cons (cons 'word
                                                                word)
                                                          token-list))))
              (else (get-tokens (cdr lst) #:token-list token-list)))))))

(define (tokenize s)
  (get-tokens (convert s)))

(define (word-tag? tok)
  (eq? (car tok) 'word))

(define (operand-tag? tok)
  (eq? (car tok) 'numeric))

(define (operator-tag? tok)
  (or (eq? (car tok) 'operator)
      (eq? (car tok) 'unicode-operator)))

(define (operator-tag=? op1 op2)
  (and (operator-tag? op1)
       (operator-tag? op2)
       (equal? (cdr op1)
               (cdr op2))))


(provide (all-defined-out))
