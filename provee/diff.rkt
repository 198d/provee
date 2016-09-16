#lang racket
(provide compute-differences
         compute-file-differences
         difference?
         has-differences?
         values-added?
         values-removed?
         make-diff
         diff-type
         diff-result
         diff-value-formatter)


(require "vendor/diff.rkt")


(struct diff (type result)
        #:transparent #:constructor-name make-diff)


(define (diff-value-formatter diff)
  (case (diff-type diff)
    ['file/mode (lambda (value) (~r value #:base 8 #:min-width 4
                                          #:pad-string "0"))]
    [else ~a]))


(define (difference? line)
  (or (values-removed? line) (values-added? line)))


(define (values-removed? line)
  (and (pair? line)
       (equal? (car line) '#:removed)))


(define (values-added? line)
  (and (pair? line)
       (equal? (car line) '#:added)))


(define (has-differences? diff)
  (and (diff? diff)
       (ormap difference?
              (diff-result diff))))


(define (compute-differences type left right [test-proc equal?])
  (make-diff
    type
    (list-diff (if (list? left) left (list left))
               (if (list? right) right (list right))
               test-proc)))


(define (compute-file-differences left right
                                  #:map-proc [map-proc port->lines]
                                  #:test-proc [test-proc string=?]
                                  #:mode [mode 'binary]
                                  #:diff-type [diff-type 'file/contents])
    (call-with-input-file
      left
      (lambda (left-port)
        (call-with-input-file
          right
          (lambda (right-port)
            (compute-differences
              diff-type (map-proc left-port) (map-proc right-port)
              test-proc))
          #:mode mode))
      #:mode mode))
