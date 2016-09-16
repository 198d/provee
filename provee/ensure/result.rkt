#lang racket
(provide make-result
         result
         result?
         result-proc
         result-succeeded?
         result-comment
         result-message
         result-diffs
         result-data
         result-failed?
         result-changed?
         result-skipped?
         format-result)


(require "../exceptions.rkt"
         "../logging.rkt")


(struct result (proc succeeded? changed? skipped? comment message
                       diffs data)
        #:transparent
        #:property prop:procedure
          (lambda (self key)
            (dict-ref (result-data self) key)))


(define (make-result proc succeeded? changed? skipped? comment
                     #:message [message #f] #:diffs [diffs (hash)]
                     #:data [data (hash)])
  (let ([result (result proc succeeded? changed? skipped? comment
                        message diffs data)])

    (cond [succeeded?
           (provee:log-info 'provee/result
                            (format-result result) #:data result)
           result]
          [else
           (provee:log-error 'provee/result
                             (format-result result) #:data result)
           (provee:raise exn:fail:provee:ensure:result
                         (format "~a: ~a" proc message)
                         result)])))


(define (result-failed? result)
  (not (result-succeeded? result)))


(define (format-result result)
  (match (struct->vector result)
    [(vector _ proc succeeded? changed? _ comment message _ _)
     (string-join (map ~a
                       (filter (negate void?)
                               (list
                                 proc
                                 (cond [(and comment message)
                                        (format "~a (~a)" comment message)]
                                       [comment comment]
                                       [message message])
                                 (if succeeded?
                                   (if changed? 
                                     "succeeded (changed)"
                                     "succeeded")
                                   "failed"))))
                  " - ")]))
