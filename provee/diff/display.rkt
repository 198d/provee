#lang racket
(provide diff->string)


(require "../diff.rkt")


(define (string-append-safe . strs)
  (apply
    string-append
    (filter string? (flatten strs))))


(define (format-lines prefix diff lines)
  (string-join
    (for/list ([line (in-list lines)])
      (format "~a ~a"
              prefix
              ((diff-value-formatter diff) line)))
    "\n"))
(define format-added-lines (curry format-lines ">"))
(define format-removed-lines (curry format-lines "<"))


(define (format-line-range start len)
  (if (> len 1)
    (format "~a,~a" start
            (+ start (- len 1)))
    (format "~a" start)))


(define (include-line-numbers? diff)
  (list? (member (diff-type diff) (list 'file/contents))))


(define (format-line-numbers left-start left-length right-start
                             right-length action)
  (let ([true-left-start (if (equal? action 'a)
                           (- left-start 1)
                           left-start)]
        [true-right-start (if (equal? action 'd)
                            (- right-start 1)
                            right-start)])
    (format "~a~a~a"
      (format-line-range true-left-start left-length)
      action
      (format-line-range true-right-start right-length))))


(define (format-added-section diff left-line-number right-line-number vals)
  (string-append-safe
    (when (include-line-numbers? diff)
      (list
        (format-line-numbers left-line-number 1 right-line-number (length vals)
                             'a)
        "\n"))
    (format-added-lines diff vals)))


(define (format-removed-section diff left-line-number right-line-number vals)
  (string-append-safe
    (when (include-line-numbers? diff)
      (list
        (format-line-numbers left-line-number (length vals) right-line-number 1
                             'd)
        "\n"))
    (format-removed-lines diff vals)))


(define (format-changed-section diff left-line-number right-line-number
                                removed-vals added-vals)
  (string-append-safe
    (when (include-line-numbers? diff)
      (list
        (format-line-numbers left-line-number (length removed-vals)
                             right-line-number (length added-vals) 'c)
        "\n"))
    (format-removed-lines diff removed-vals)
    "\n---\n"
    (format-added-lines diff added-vals)))


(define (diff->string diff)
  (let ([result (list)])
    (let loop ([current-section (void)]
               [left-line-number 1]
               [right-line-number 1]
               [list-diff (diff-result diff)])
      (unless (void? current-section)
        (set! result (append result (list current-section))))
      (cond [(and (pair? list-diff) (difference? (car list-diff)))
             (cond [(values-added? (car list-diff))
                    (loop
                      (format-added-section diff
                                            left-line-number
                                            right-line-number
                                            (cdar list-diff))
                      left-line-number
                      (+ right-line-number (length (cdar list-diff)))
                      (cdr list-diff))]
                   [(values-removed? (car list-diff))
                    (cond [(and (pair? (cdr list-diff))
                                (values-added? (cadr list-diff)))
                           (loop
                             (format-changed-section diff
                                                     left-line-number
                                                     right-line-number
                                                     (cdar list-diff)
                                                     (cdadr list-diff))
                             (+ left-line-number (length (cdar list-diff)))
                             (+ right-line-number (length (cdadr list-diff)))
                             (cddr list-diff))]
                          [else
                           (loop
                             (format-removed-section diff
                                                     left-line-number
                                                     right-line-number
                                                     (cdar list-diff))
                             (+ left-line-number (length (cdar list-diff)))
                             right-line-number
                             (cdr list-diff))])])]
            [(pair? list-diff)
             (loop (void) (+ left-line-number 1) (+ right-line-number 1)
                   (cdr list-diff))]))
    (string-join result "\n")))
