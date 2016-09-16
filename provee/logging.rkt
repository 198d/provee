#lang racket
(provide provee:logger
         provee:log-message
         provee:log-fatal
         provee:log-error
         provee:log-warning
         provee:log-info
         provee:log-debug
         format-log-message
         display-log-message
         make-log-receiver-thread)


(require racket/date
         (for-syntax syntax/parse))


(define provee:logger (make-logger 'provee))


(begin-for-syntax
  (define-splicing-syntax-class maybe-data
    (pattern (~seq #:data data:expr))
    (pattern (~seq) #:with data #''#f))
  (define (make-leveled-log-message-syntax log-level stx)
    (with-syntax ([level log-level])
      (syntax-parse stx
        [(_ topic message data:maybe-data)
         #'(provee:log-message level topic message data.data)]
        [(_ message data:maybe-data)
         #'(provee:log-message level 'provee message data.data)]))))


(define-syntax (provee:log-message stx)
  (syntax-case stx ()
    [(_ level topic message data)
     #'(when (log-level? provee:logger level topic)
         (log-message provee:logger level topic message
                      (vector (current-date) data) #f))]))


(define-syntax (provee:log-fatal stx)
  (make-leveled-log-message-syntax #''fatal stx))
(define-syntax (provee:log-warning stx)
  (make-leveled-log-message-syntax #''warning stx))
(define-syntax (provee:log-error stx)
  (make-leveled-log-message-syntax #''error stx))
(define-syntax (provee:log-info stx)
  (make-leveled-log-message-syntax #''info stx))
(define-syntax (provee:log-debug stx)
  (make-leveled-log-message-syntax #''debug stx))


(define (format-log-message date level topic message)
  (parameterize ([date-display-format 'iso-8601])
    (format "~a | ~a | ~a"
            (date->string date #t)
            (~a (format "~a (~a)"
                        (string-upcase (symbol->string level)) topic)
                #:width 25)
            message)))


(define (display-log-message date level topic message)
  (displayln (format-log-message date level topic message)
             (current-error-port)))


(define (make-log-receiver-thread receiver message-handler)
  (let* ([receiver-controller (make-channel)]
         [sync-events (curry sync receiver receiver-controller)]
         [sync-receiver/timeout (curry sync/timeout 0 receiver)]
         [receiver-thread
          (thread
            (lambda ()
              (let loop ([message (sync-events)]
                         [draining? #f])
                (match message
                  [(vector level message (vector date data) topic)
                   (message-handler level message date data topic)]
                  [(? symbol? command)
                   (loop (sync-receiver/timeout) #t)]
                  [#f (kill-thread (current-thread))])
                  (loop (if draining?
                          (sync-receiver/timeout)
                          (sync-events))
                        draining?))))])
    (values receiver-thread receiver-controller)))
