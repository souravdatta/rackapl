#lang racket

(require "syms.rkt")
(require racket/gui)
(require racket/draw)

(define (make-bitmap-sym sym)
  (let* ([bitmap (make-bitmap 40 40)]
         [dc (new bitmap-dc% [bitmap bitmap])])
    (send dc set-font (make-font #:size 18 #:weight 'bold))
    (send dc draw-text sym 10 4)
    bitmap))

(define (make-button parent s callback)
  (new button%
       [parent parent]
       [label (make-bitmap-sym s)]
       [min-width 40]
       [min-height 40]
       [callback callback]))

(define (create-paned-buttons parent syms cols callback-fn)
  (let ([vpane (new vertical-pane% [parent parent])])
    (let looper ([symbols syms]
                 [buttons '()]
                 [hpanes '()])
      (cond
        ((= (length symbols) 0) (values buttons vpane hpanes))
        ((< (length symbols) cols)
         (let* ([hpane (new horizontal-pane%
                            [parent vpane])]
                [btns (map (λ (s)
                             (make-button hpane s (callback-fn s))) symbols)])
           (values (append buttons btns)
                   vpane
                   (append hpanes (list hpane)))))
        (else (let* ([hpane (new horizontal-pane%
                                 [parent vpane])]
                     [btns (map (λ (s)
                                  (make-button hpane s (callback-fn s))) (take symbols cols))])
                (looper (drop symbols cols)
                        (append buttons btns)
                        (append hpanes (list hpane)))))))))


(define (gui)
  (define frm (new frame%
                   [label "APL"]
                   [stretchable-width #f]
                   [stretchable-height #f]))
  (define main-pane (new horizontal-pane%
                         [parent frm]))
  (define editor-pane (new vertical-pane%
                           [parent main-pane]))
  (define input-pane (new horizontal-pane%
                          [parent editor-pane]))
  (define input-box (new text-field%
                         [parent input-pane]
                         [label ""]
                         [style '(multiple hscroll)]
                         [min-height 120]
                         [font (make-font #:size 12)]))
  (define do-button (new button%
                         [label "Do it"]
                         [parent input-pane]
                         [callback (λ (b e)
                                     (send (send result-box get-editor) insert (send input-box get-value))
                                     (send (send result-box get-editor) insert (format "~%"))
                                     (send the-clipboard set-clipboard-string
                                           (send input-box get-value)
                                           (send e get-time-stamp))
                                     (send input-box set-value "")
                                     (send input-box focus))]))
  (define-values (buttons vpane hpanes)
    (create-paned-buttons editor-pane unicode-symbols 10
                          (λ (s)
                            (λ (b e)
                              (send (send input-box get-editor) insert s)
                              (send input-box focus)))))
  (define result-pane (new vertical-pane%
                           [parent main-pane]))
  (define result-box (new text-field%
                          [parent result-pane]
                          [label ""]
                          [style '(multiple hscroll)]
                          [min-width 400]
                          [font (make-font #:size 14)]
                          [enabled #f]))
  (define edit-checkbox (new check-box%
                             [parent result-pane]
                             [label "Edit"]
                             [value #f]
                             [callback (λ (c e)
                                         (if (send c get-value)
                                             (send result-box enable #t)
                                             (send result-box enable #f)))]))
  (send input-box focus)
  (send frm show #t))


(gui)
