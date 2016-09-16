#lang racket
(provide
  provee:raise
  provee:raise/system-call
  catch-ensure-result
  exn:fail:provee
  exn:fail:provee?
  exn:fail:provee:ensure
  exn:fail:provee:ensure?
  exn:fail:provee:ensure:result
  exn:fail:provee:ensure:result?
  exn:fail:provee:ensure:result-result)


(require ffi/unsafe)


(define (provee:raise exception message . args)
  (raise (apply exception message (current-continuation-marks) args)))


(define (provee:raise/system-call function)
  (provee:raise exn:fail:provee
                (format "System call (~a) failed; errno ~a"
                        function (saved-errno))))


(define-syntax (catch-ensure-result stx)
  (datum->syntax
    stx
    `(with-handlers ([exn:fail:provee:ensure:result?
                      (lambda (exc) (exn:fail:provee:ensure:result-result
                                      exc))])
                    ,(cadr (syntax->list stx)))))


(struct exn:fail:provee exn:fail ())
(struct exn:fail:provee:ensure exn:fail:provee ())
(struct exn:fail:provee:ensure:result exn:fail:provee:ensure (result))
