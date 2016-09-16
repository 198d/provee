#lang racket
(provide list-installed
         install-package
         package-installed?
         available?)


(require "../exec.rkt"
         "../exceptions.rkt")


(define (available?)
  (andmap path? (map find-executable-path
                     (list "pkgin" "pkg_info"))))


(define (package-installed? name)
  (let-values ([(exit-code stdout stderr) (run-command 'pkg_info '-E name)])
    (zero? exit-code)))


(define (list-installed)
  (let-values ([(exit-code stdout stderr) (run-command 'pkgin 'list)])
    (unless (zero? exit-code)
      (provee:raise exn:fail:provee "failed to get package list"))
    (for/list ([line (in-list (string-split stdout "\n" #:repeat? #t))])
      (car (string-split line " " #:repeat? #t)))))


(define (install-package name)
  (run-command 'pkgin '-y 'install name))
