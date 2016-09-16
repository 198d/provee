#lang racket
(provide list-installed
         install-package
         package-installed?
         available?)


(require "../exec.rkt"
         "../exceptions.rkt")


(define (available?)
  (andmap path? (map find-executable-path
                     (list "dnf"))))


(define (package-installed? name) #f)


(define (list-installed)
  (let-values ([(exit-code stdout stderr) (run-command 'dnf 'list 'installed)])
    (unless (zero? exit-code)
      (provee:raise exn:fail:provee "failed to get package list"))
    (for/list ([tokens (sequence-map
                         (lambda (line)
                           (string-split line " " #:repeat? #t))
                         (in-lines
                           (open-input-string stdout)))]
               #:when (= (length tokens) 3))
      (car tokens))))


(define (install-package name)
  (run-command 'dnf 'install '--assumeyes name))
