#lang racket
(provide ensure:file/contents
         ensure:file/attributes
         ensure:file/directory
         ensure:file/symlink
         ensure:system-package/installed
         ensure:pkgin-package/installed
         ensure:dnf-package/installed
         ensure:apt-package/installed
         ensure:service/started
         ensure:service/stopped
         ensure:service/restarted
         ensure:systemd-service/started
         ensure:systemd-service/stopped
         ensure:systemd-service/restarted
         ensure:exec
         ensure:exec/shell
         ensure:together)


(require (prefix-in ensure: "ensure/file.rkt")
         (prefix-in ensure: "ensure/package.rkt")
         (prefix-in ensure: "ensure/service.rkt")
         (prefix-in ensure: "ensure/exec.rkt")
         "logging.rkt"
         "ensure/result.rkt"
         (for-syntax syntax/parse))


(define-syntax (ensure:together stx)
  (syntax-parse stx
    [(_ body ...)
     (with-syntax ([receiver-controller (gensym)]
                   [receiver-thread (gensym)]
                   [results (gensym)])
       #'(let*-values ([(results) '()]
                       [(receiver-thread receiver-controller)
                        (make-log-receiver-thread
                          (make-log-receiver
                            provee:logger 'info 'provee/result)
                          (lambda (message level date data topic)
                            (set! results (append results (list data)))))])
           body ...
           (channel-put receiver-controller 'exit)
           (result
             'together (andmap result-succeeded?
                               results)
             (ormap result-changed? results)
             (andmap result-skipped? results)
             #f #f (hash) (hash 'results results))))]))
