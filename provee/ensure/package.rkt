#lang racket
(provide system-package/installed
         dnf-package/installed
         apt-package/installed)


(require "../exec.rkt"
         (only-in "../diff.rkt" compute-differences has-differences?)
         "../exceptions.rkt"
         "result.rkt"
         (prefix-in dnf: "../package/dnf.rkt")
         (prefix-in apt: "../package/apt.rkt"))


(define (make-installed-proc proc-name package-installed? list-installed
                          install-package)
  (lambda (name #:comment [comment #f])
    (unless comment
      (set! comment (format "Ensure ~a package is installed" name)))
    (cond [(package-installed? name)
           (make-result proc-name #t #f #f comment)]
          [else
           (let*-values ([(before-installed) (list-installed)]
                         [(exit-code stdout stderr)
                          (install-package name)]
                         [(after-installed) (list-installed)]
                         [(installed-differences)
                          (compute-differences
                            'package/list
                            before-installed
                            after-installed)]
                         [(has-installed-differences?)
                          (has-differences? installed-differences)])
             (unless (zero? exit-code)
               (make-result proc-name #f #f #f comment
                            #:message "package installation failed"
                            #:data (hash 'exit-code exit-code
                                         'stdout stdout
                                         'stderr stderr)))
             (make-result
               proc-name #t has-installed-differences? #f
               comment #:diffs (if has-installed-differences?
                                 (hash 'installed installed-differences)
                                 (hash))
               #:data (hash 'exit-code exit-code
                            'stdout stdout
                            'stderr stderr)))])))


(define (system-package/installed name #:comment [comment #f])
  ((cond [(dnf:available?) dnf-package/installed]
         [(apt:available?) apt-package/installed]
         [else (provee:raise exn:fail:provee:ensure
                             "cannot find a supported package manager")])
    name #:comment comment))


(define dnf-package/installed (make-installed-proc 'dnf-package/installed
                                                   dnf:package-installed?
                                                   dnf:list-installed
                                                   dnf:install-package))


(define apt-package/installed (make-installed-proc 'apt-package/installed
                                                   apt:package-installed?
                                                   apt:list-installed
                                                   apt:install-package))
