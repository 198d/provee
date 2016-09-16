#lang racket
(provide file/contents
         file/directory
         file/attributes
         file/symlink)


(require openssl/sha1
         (only-in "result.rkt" make-result)
         (only-in "../diff.rkt" compute-differences
                  compute-file-differences has-differences?)
         "../file.rkt"
         "../logging.rkt")


(define (file/directory path #:comment [comment #f])
  (let ([true-path (path->absolute-path path)])
    (unless comment
      (set! comment (format "Ensure directory ~a exists" true-path)))
    (if (directory-exists? true-path)
      (make-result 'file/directory #t #f #f comment
                   #:data (hash 'path true-path))
      (and (make-directory* true-path)
           (make-result 'file/directory #t #t #f comment
                        #:data (hash 'path true-path))))))


(define (file/symlink target source #:comment [comment #f])
  (let ([source-path (path->absolute-path source)]
        [target-path (path->absolute-path target)])
    (unless comment
      (set! comment (format "Ensure ~a points to ~a" target-path source-path)))
    (if (link-exists? target-path)
      (make-result 'file/symlink #t #f #f comment
                   #:data (hash 'path target-path))
      (with-handlers ([exn:fail? 
                       (lambda (e) 
                         (make-result 'file/symlink #f #f #f
                                      comment #:message (exn-message e)))])
        (make-file-or-directory-link source target)
        (make-result 'file/symlink #t #t #f comment
                     #:data (hash 'path target-path))))))


(define (file/attributes filename #:mode [mode #f] #:owner [owner #f]
         #:group [group #f] #:comment [comment #f])
  (unless comment
    (set! comment
      (format "Ensure correct ~a for ~a"
              (string-join
                (map cdr (filter car (list (cons mode "mode")
                                           (cons owner "owner")
                                           (cons group "group"))))
                ", ")
              filename)))
  (let ([file-path (path->absolute-path filename)]
        [current-mode (file-or-directory-permissions filename 'bits)]
        [current-owner (file-or-directory-owner filename)]
        [current-group (file-or-directory-group filename)]
        [diffs (make-hash)])

    (define (diff-and-apply diff-key diff-mode current-value proposed-value
                            apply-fn)
      (when (and proposed-value (not (equal? current-value proposed-value)))
        (apply-fn proposed-value)
        (hash-set! diffs diff-key
                   (compute-differences
                     diff-mode current-value proposed-value))))

    (diff-and-apply 'mode 'file/mode current-mode mode
                    (curry file-or-directory-permissions file-path))
    (diff-and-apply 'owner 'file/owner current-owner (~a owner)
                    (curry file-or-directory-owner file-path))
    (diff-and-apply 'group 'file/group current-group (~a group)
                    (curry file-or-directory-group file-path))

    (make-result
       'file/attributes #t (not (hash-empty? diffs)) #f comment
       #:diffs diffs #:data (hash 'path file-path))))



(define (file/contents [filename #f] #:contents [contents #f]
         #:sha1sum [sha1sum #f] #:binary? [binary? #f] #:comment [comment #f])
  (unless (or filename contents)
    (make-result
      'file/contents #f #f #f comment
      #:message "at least one of filename or contents must be provided"))

  (let*-values ([(temp-file?) (not filename)]
                [(temp-destination) (when contents (make-temporary-file))]
                [(final-destination) (if temp-file?
                                       (make-temporary-file)
                                       (path->absolute-path filename))]
                [(new-file?) (or temp-file?
                                 (not (file-exists? final-destination)))]
                [(current-mode current-owner current-group)
                 (if new-file?
                   (values #f #f #f)
                   (current-file-or-directory-attributes final-destination))])

    (unless comment
      (set! comment (format "Ensure contents of ~a" final-destination)))

    (when contents
      (call-with-output-file
        temp-destination
        (lambda (out-port)
          ((cond [(string? contents) write-string]
                 [(bytes? contents) write-bytes]
                 [(input-port? contents) copy-port]) contents out-port))
        #:exists 'truncate/replace)
      (when (input-port? contents) (close-input-port contents)))

    (when sha1sum
      (unless (checksum-matches?
                (if contents temp-destination final-destination)
                sha1sum)
        (make-result 'file/contents #f #f #f comment
                   #:message "sha1sum does not match contents")))
    (let ([diffs
           (for/hash ([(key diff)
                       (in-hash
                         (hash 'contents
                               (when (and contents (not binary?))
                                 (compute-file-differences
                                   (if new-file?
                                     (string->path "/dev/null")
                                     final-destination)
                                   temp-destination
                                   #:mode 'text))
                               'sha1sum
                               (when (and contents (not new-file?))
                                 (compute-file-differences
                                   final-destination
                                   temp-destination
                                   #:map-proc sha1
                                   #:diff-type 'file/checksum))))]
                     #:when (has-differences? diff))
             (values key diff))])

        (define (state-changed?)
          (or (not (hash-empty? diffs))
              new-file?))

        (cond [(and (state-changed?) contents)
               (when (not temp-file?)
                 (file-or-directory-permissions temp-destination
                                                current-mode)
                 (file-or-directory-owner temp-destination current-owner)
                 (file-or-directory-group temp-destination current-group))
               (rename-file-or-directory temp-destination
                                         final-destination #t)]
              [else (delete-file temp-destination)])

        (make-result 'file/contents #t (state-changed?) #f comment
                     #:diffs (make-immutable-hash (hash->list diffs))
                     #:data (hash 'path final-destination)))))
