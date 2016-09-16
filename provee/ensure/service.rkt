#lang racket
(provide systemd-service/started
         systemd-service/stopped
         systemd-service/restarted
         service/started
         service/stopped
         service/restarted)


(require "../exceptions.rkt"
         "result.rkt"
         (prefix-in systemd: "../service/systemd.rkt"))


(define (systemd-service/started name #:comment [comment #f])
  (unless comment
    (set! comment (format "Ensure ~a service is started" name)))
  (let ([current-state (systemd:service-state name)])
    (if (eq? current-state 'running)
      (make-result 'systemd-service/started #t #f #t comment)
      (let-values ([(exit-code stdout stderr) (systemd:start-service name)])
        (make-result 'systemd-service/started (= exit-code 0) #t #f comment
                     #:data (hash 'exit-code exit-code 'stdout stdout
                                  'stderr stderr))))))


(define (systemd-service/stopped name #:comment [comment #f])
  (unless comment
    (set! comment (format "Ensure ~a service is stopped" name)))
  (let ([current-state (systemd:service-state name)])
    (if (eq? current-state 'stopped)
      (make-result 'systemd-service/stopped #t #f #t comment)
      (let-values ([(exit-code stdout stderr) (systemd:stop-service name)])
        (make-result 'systemd-service/stopped (= exit-code 0) #t #f comment
                     #:data (hash 'exit-code exit-code 'stdout stdout
                                  'stderr stderr))))))


(define (systemd-service/restarted name #:comment [comment #f])
  (unless comment
    (set! comment (format "Ensure ~a service is restarted" name)))
  (let-values ([(exit-code stdout stderr) (systemd:restart-service name)])
    (make-result 'systemd-service/restarted (= exit-code 0) #t #f comment
                 #:data (hash 'exit-code exit-code 'stdout stdout
                              'stderr stderr))))


(define (service/started name #:comment [comment #f])
  ((cond [(systemd:available?) systemd-service/started]
         [else (provee:raise exn:fail:provee:ensure
                             "cannot find supported service manager")])
   name #:comment comment))


(define (service/stopped name #:comment [comment #f])
  ((cond [(systemd:available?) systemd-service/stopped]
         [else (provee:raise exn:fail:provee:ensure
                             "cannot find supported service manager")])
   name #:comment comment))


(define (service/restarted name #:comment [comment #f])
  ((cond [(systemd:available?) systemd-service/restarted]
         [else (provee:raise exn:fail:provee:ensure
                             "cannot find supported service manager")])
   name #:comment comment))
