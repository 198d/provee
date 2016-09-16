#lang racket
(provide list-installed
         install-package
         package-installed?
         available?)


(require "../exec.rkt"
         "../exceptions.rkt")


(define (available?)
  (andmap path? (map find-executable-path
                     (list "apt-get" "dpkg-query"))))


(define (package-installed? name) #f)


(define (list-installed)
  (let-values ([(exit-code stdout stderr)
                (run-command 'dpkg-query '-W '-f "${binary:Package}\\n")])
    (unless (zero? exit-code)
      (provee:raise exn:fail:provee "failed to get package list"))
    (string-split stdout "\n")))


(define (install-package name)
  (run-command 'apt-get 'install '--assume-yes name))
