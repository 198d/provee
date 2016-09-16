#lang racket
(provide available?
         start-service
         stop-service
         enable-service
         disable-service
         restart-service
         service-state)


(require "../exec.rkt")


(define (available?)
  (path? (find-executable-path "systemctl")))


(define (start-service name)
  (run-command 'systemctl 'start name))


(define (stop-service name)
  (run-command 'systemctl 'stop name))


(define (enable-service name)
  (run-command 'systemctl 'enable name))


(define (disable-service name)
  (run-command 'systemctl 'disable name))


(define (restart-service name)
  (run-command 'systemctl 'restart name))


(define (service-state name)
  (let-values ([(exit-code stdout stderr)
                (run-command 'systemctl 'is-active name)])
    (if (= exit-code 0)
      'running
      'stopped)))

