#lang racket


(define (operand? tok)
  (eq? (car tok) 'numeric))

(define (operator? tok)
  (or (eq? (car tok) 'operator)
      (eq? (car tok) 'unicode-operator)))

(define (create-or-append-arg tok arg-stack)
  (cond
    ((empty? arg-stack) (list (list (cdr tok))))
    (else (cons (cons (cdr tok) (car arg-stack)) (cdr arg-stack)))))

(define (consume op arg-stack)
  (list '() (list (format "~a/ ~a" (cdr op) arg-stack))))


(define (apleval tokens arg-stack op-stack)
  (cond
    ((empty? tokens) (list op-stack arg-stack))
    ((operand? (car tokens)) (apleval (cdr tokens)
                                      (create-or-append-arg (car tokens) arg-stack)
                                      op-stack))
    ((operator? (car tokens))
     (cond
       ((empty? op-stack) (apleval (cdr tokens)
                                   (cons '() arg-stack)
                                   (cons (car tokens) op-stack)))
       (else (if (operator? (car op-stack))
                 (let* ([op (car op-stack)]
                        [op-stack (cdr op-stack)])
                   (apleval (cdr tokens) (consume op arg-stack) (cons (car tokens) op-stack)))
                 (apleval (cdr tokens) (cons '() arg-stack) (cons (car tokens) op-stack))))))))
