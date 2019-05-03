#lang racket

(require "reader.rkt")

(define (create-or-append-arg tok arg-stack)
  (cond
    ((empty? arg-stack) (list (list (cdr tok))))
    (else (cons (cons (cdr tok) (car arg-stack)) (cdr arg-stack)))))

(define (remove-empties l)
  (filter (λ (x) (not (empty? x))) l))

(define (apl-apply op arg-stack)
  (list '() (list 'apl-apply
                  (cdr op)
                  (remove-empties arg-stack))))

(define (extract-tokens for-op match-op tokens)
  (define (extract-tokens-aux cur-tokens extracted-tokens count-op)
    ;(displayln (format "CURTOKENS: ~a~%EXTRACTED: ~a~%" cur-tokens extracted-tokens))
    (if (= count-op 0)
        (values (reverse (cdr extracted-tokens))
                cur-tokens)
        (cond
          ((empty? cur-tokens) (error (format "No matching ~a ~a found" (cdr match-op) count-op)))
          ((operator-tag=? (car cur-tokens) match-op)
           (extract-tokens-aux (cdr cur-tokens) (cons (car cur-tokens) extracted-tokens) (- count-op 1)))
          ((operator-tag=? (car cur-tokens) for-op)
           (extract-tokens-aux (cdr cur-tokens) (cons (car cur-tokens) extracted-tokens) (+ count-op 1)))
          (else (extract-tokens-aux (cdr cur-tokens) (cons (car cur-tokens) extracted-tokens) count-op)))))
  (extract-tokens-aux (cdr tokens) '() 1))

(define (apleval tokens arg-stack op-stack)
  (cond
    ((empty? tokens) (apl-apply (if
                                 (empty? op-stack) '(operator . "just") (car op-stack))
                                arg-stack))
    ((or (word-tag? (car tokens))
         (operand-tag? (car tokens))) (apleval (cdr tokens)
                                           (create-or-append-arg (car tokens) arg-stack)
                                           op-stack))
    ((operator-tag? (car tokens))
     (cond
       ((operator-tag=? (car tokens)
                    '(operator . ")"))
        (let-values ([(etokens rtokens) (extract-tokens (car tokens)
                                                        '(operator . "(")
                                                        tokens)])
          (let ([result (apleval etokens '() '())])
            (apleval rtokens (append result arg-stack) op-stack))))
       ((empty? op-stack) (apleval (cdr tokens)
                                   (cons '() arg-stack)
                                   (cons (car tokens) op-stack)))
       (else (if (operator-tag? (car op-stack))
                 (let* ([op (car op-stack)]
                        [op-stack (cdr op-stack)])
                   (apleval (cdr tokens) (apl-apply op arg-stack) (cons (car tokens) op-stack)))
                 (apleval (cdr tokens) (cons '() arg-stack) (cons (car tokens) op-stack))))))))

(define (apleval-s s)
  (remove-empties (apleval (tokenize s) '() '())))

(define (apl-eval tokens env)
  (remove-empties (apleval tokens '() '())))


