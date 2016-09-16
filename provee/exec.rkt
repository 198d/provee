#lang racket
(provide exhaust-process-ports
         run-command)


(require "exceptions.rkt")


(define (copy-port-and-close input-port . output-ports)
  (apply copy-port input-port output-ports)
  (close-input-port input-port)
  (map close-output-port output-ports))


(define (exhaust-process-ports stdin stdout stderr #:input [input #""])
  (let* ([input-port (cond [(input-port? input) input]
                           [(string? input)
                            (open-input-string input)]
                           [(bytes? input)
                            (open-input-bytes input)]
                           [else (open-input-bytes #"")])]
         [stdout-output (open-output-bytes "")]
         [stderr-output (open-output-bytes "")]
         [threads
          (list
            (thread (thunk (copy-port-and-close input-port stdin)))
            (thread (thunk (copy-port-and-close stdout stdout-output)))
            (thread (thunk (copy-port-and-close stderr stderr-output))))])
    (map thread-wait threads)
    (apply values
           (map get-output-string
                (list stdout-output stderr-output)))))


(define (run-command #:stdin [stdin #f] #:shell? [shell? #f] . args)
  (let* ([string-args (for/list ([arg (in-list args)])
                        (format "~a" arg))]
         [command (car string-args)]
         [rest-args (cdr string-args)]
         [executable (if (or (file-exists? command) shell?)
                       command
                       (find-executable-path command))]
         [process-proc (if shell? process process*)])

    (when (not executable)
      (provee:raise exn:fail:provee "could not find executable"))

    (let*-values ([(out-port in-port pid err-port cntrl)
                   (apply values (apply process-proc executable
                                        (cdr string-args)))]
                  [(stdout stderr)
                   (exhaust-process-ports in-port out-port err-port
                                          #:input stdin)])
      (cntrl 'wait)
      (values (cntrl 'exit-code) stdout stderr))))
