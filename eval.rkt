#lang racket

(require "reader.rkt")

(define (operand? tok)
  (eq? (car tok) 'numeric))

(define (operator? tok)
  (or (eq? (car tok) 'operator)
      (eq? (car tok) 'unicode-operator)))

(define (create-or-append-arg tok arg-stack)
  (cond
    ((empty? arg-stack) (list (list (cdr tok))))
    (else (cons (cons (cdr tok) (car arg-stack)) (cdr arg-stack)))))

(define (remove-empties l)
  (filter (λ (x) (not (empty? x))) l))

(define (consume op arg-stack)
  (list '() (list (format "~a/ ~a" (cdr op) (remove-empties arg-stack)))))

(define (operator=? op1 op2)
  (and (operator? op1)
       (operator? op2)
       (equal? (cdr op1)
               (cdr op2))))

(define (extract-tokens for-op match-op tokens)
  (define (extract-tokens-aux cur-tokens extracted-tokens count-op)
    ;(displayln (format "CURTOKENS: ~a~%EXTRACTED: ~a~%" cur-tokens extracted-tokens))
    (if (= count-op 0)
        (values (reverse (cdr extracted-tokens))
                cur-tokens)
        (cond
          ((empty? cur-tokens) (error (format "No matching ~a ~a found" (cdr match-op) count-op)))
          ((operator=? (car cur-tokens) match-op)
           (extract-tokens-aux (cdr cur-tokens) (cons (car cur-tokens) extracted-tokens) (- count-op 1)))
          ((operator=? (car cur-tokens) for-op)
           (extract-tokens-aux (cdr cur-tokens) (cons (car cur-tokens) extracted-tokens) (+ count-op 1)))
          (else (extract-tokens-aux (cdr cur-tokens) (cons (car cur-tokens) extracted-tokens) count-op)))))
  (extract-tokens-aux (cdr tokens) '() 1))

(define (apleval tokens arg-stack op-stack)
  (cond
    ((empty? tokens) (consume (if
                               (empty? op-stack) '(operator . "just") (car op-stack))
                              arg-stack))
    ((operand? (car tokens)) (apleval (cdr tokens)
                                      (create-or-append-arg (car tokens) arg-stack)
                                      op-stack))
    ((operator? (car tokens))
     (cond
       ((operator=? (car tokens)
                    '(operator . ")"))
        (let-values ([(etokens rtokens) (extract-tokens (car tokens)
                                                        '(operator . "(")
                                                        tokens)])
          (let ([result (apleval etokens '() '())])
            (apleval rtokens (append result arg-stack) op-stack))))
       ((empty? op-stack) (apleval (cdr tokens)
                                   (cons '() arg-stack)
                                   (cons (car tokens) op-stack)))
       (else (if (operator? (car op-stack))
                 (let* ([op (car op-stack)]
                        [op-stack (cdr op-stack)])
                   (apleval (cdr tokens) (consume op arg-stack) (cons (car tokens) op-stack)))
                 (apleval (cdr tokens) (cons '() arg-stack) (cons (car tokens) op-stack))))))))

(define (apleval-s s)
  (remove-empties (apleval (tokenize s) '() '())))

