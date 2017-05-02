#lang racket
(provide provee:initialize!
         provee:finalize!
         provee:start-logging!
         provee:stop-logging!
         provee:script)


(require "logging.rkt"
         "exceptions.rkt"
         (prefix-in ensure: "ensure/result.rkt")
         "diff/display.rkt"
         (for-syntax syntax/parse racket/match)
         racket/runtime-path)


(define provee:current-log-receiver (make-parameter #f))


(define (provee:start-logging! log-level)
  (let*-values ([(receiver-thread receiver-controller)
                 (make-log-receiver-thread
                   (make-log-receiver provee:logger log-level)
                   (lambda (level message date data topic)
                     (display-log-message date level topic message)
                     (match data
                       [(? ensure:result?)
                        (for ([(name differences)
                               (in-hash (ensure:result-diffs data))])
                          (display-log-message
                            date level topic (format "    ~a differences:"
                                                     name))
                          (for ([line
                                 (in-list (string-split
                                            (diff->string differences)
                                            "\n"))])
                            (display-log-message
                              date level topic (format "        ~a" line))))
                        (when (ensure:result-failed? data)
                          (display-log-message
                            date level topic (~v data)))]
                       [else (void)])))])
    (provee:current-log-receiver
      (cons receiver-thread receiver-controller))))


(define (provee:stop-logging!)
  (match (provee:current-log-receiver)
    [(cons receiver-thread receiver-controller)
     (when (thread-running? receiver-thread)
       (channel-put receiver-controller 'exit)
       (thread-wait receiver-thread))]))


(define (provee:initialize! #:log-level [log-level 'info])
  (provee:start-logging! log-level))


(define (provee:finalize!)
  (provee:stop-logging!))


(define-syntax (provee:script stx)
  (define-splicing-syntax-class runtime-paths-option
    (pattern (~seq #:runtime-paths (((~seq name:id path:expr)) ...))))
  (define-splicing-syntax-class script-arguments
    (pattern (~seq (((~seq name:id default:expr description:expr)) ...))))
  (syntax-parse stx
    [(_ script-name:id
        (~or (~optional arguments:script-arguments)
             (~optional runtime-paths:runtime-paths-option)) ... clause ...)
     (let* ([arguments (map list (attribute arguments.name)
                                 (attribute arguments.default)
                                 (attribute arguments.description))]
            [argument-parameters
             (for/list ([(argument) arguments])
               (list (datum->syntax stx
                       (string->symbol
                         (format "~a-~a"
                           (syntax->datum (attribute script-name))
                           (syntax->datum (list-ref argument 0)))))
                     (list-ref argument 1)))])
       (quasisyntax/loc stx
         (begin
           (require (only-in racket/runtime-path define-runtime-paths))
           (define-runtime-paths
             #,(or (attribute runtime-paths.name) #'())
             (values #,@(or (attribute runtime-paths.path) #'())))
           #,@(for/list ([(argument) argument-parameters])
                (match argument
                  [(list name default)
                   `(define ,name (make-parameter ,default))]))
           (command-line
             #:once-each
             #,@(for/list ([(values)
                            (map list arguments argument-parameters)])
                  (match values
                    [(list (list name default desc) (list param-name _))
                     (let* ([option (datum->syntax stx
                                      (format "--~a" (syntax->datum name)))])
                       `((,option) ,name ,desc (,param-name ,name)))])))
           (void
             (provee:initialize!
               #:log-level 'info)
             (with-handlers
               ([(lambda (e) #t) (lambda (e)
                                   (provee:finalize!)
                                   (raise e))])
               clause ...)
             (provee:finalize!)))))]))
