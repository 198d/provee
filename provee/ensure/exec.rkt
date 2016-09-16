#lang racket
(provide exec
         exec/shell)


(require "result.rkt"
         "../exec.rkt"
         "../exceptions.rkt")


(define (make-exec-proc proc-name shell?)
  (lambda (#:comment [comment #f] #:stdin [stdin #f] . args)
    (unless comment
      (set! comment (format "Ensure ~a executes" args)))
    (let-values ([(exit-code stdout stderr)
                  (with-handlers ([exn:fail:provee?
                                   (lambda (e)
                                     (make-result
                                       'ensure/exec #f #f #f comment
                                        #:message (exn-message e)))])
                    (apply
                      run-command #:shell? shell? #:stdin stdin args))])
        (make-result
          proc-name (zero? exit-code) #t #f comment
          #:data (hash 'stdout stdout 'stderr stderr
                       'exit-code exit-code)
          #:message (if (zero? exit-code)
                      #f
                      "process exited with non-zero exit code")))))


(define exec (make-exec-proc 'exec #f))
(define exec/shell (make-exec-proc 'exec/shell #t))
