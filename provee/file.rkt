#lang racket
(provide open-url
         render-template
         checksum-matches?
         file-or-directory-owner
         file-or-directory-group
         path->absolute-path
         user-home-directory
         current-file-or-directory-attributes)


(require net/url
         net/head
         threading
         scribble/text
         (prefix-in scribble: scribble/reader)
         openssl/sha1
         ffi/unsafe
         "exceptions.rkt"
         "file/ffi.rkt"
         (for-syntax syntax/parse))


(define (user-home-directory username)
  (let ([passwd-entry (getpwnam username)])
    (if passwd-entry
      (string->path (passwd-dir passwd-entry))
      (error "User does not exist in password database"))))


(define (open-url url #:max-redirects [max-redirects 5])
  (let*-values ([(status-line headers input-port)
                 (http-sendrecv/url (string->url url))]
                [(status-code) (~> status-line
                                   bytes->string/utf-8
                                   string-split
                                   second
                                   string->number)])
    (if (and (>= status-code 300) (< status-code 400))
      (if (> max-redirects 0)
        (let ([location
                (bytes->string/utf-8
                  (findf
                    bytes?
                    (map (curry extract-field #"Location") headers)))])
          (close-input-port input-port)
          (open-url location #:max-redirects (- max-redirects 1)))
        (error "open-url: max-redirects reached"))
      input-port)))


(define (checksum-matches? path expected-sum [hash-proc sha1])
  (call-with-input-file
    path
    (lambda (in-port)
      (string=? expected-sum
                (hash-proc in-port)))))


(define-syntax (render-template stx)
  (define-splicing-syntax-class maybe-bindings
    (pattern (((~seq name:id value:expr)) ...))
    (pattern (~seq) #:with (name ...) #'() #:with (value ...) #'()))
  (syntax-parse stx
    [(_ bindings:maybe-bindings template:expr)
     (with-syntax ([namespace (gensym)])
       #`(let ([namespace (make-base-namespace)]
               [syntax (call-with-input-file
                         template
                         (lambda (input-port)
                           (scribble:read-syntax-inside
                             template input-port)))])
          #,@(for/list ([binding
                         (in-list (map
                                    cons
                                    (syntax->datum #'(bindings.name ...))
                                    (syntax->datum #'(bindings.value ...))))])
               #`(namespace-set-variable-value!
                   (quote #,(datum->syntax stx (car binding)))
                   #,(datum->syntax stx (cdr binding))
                   #f
                   namespace))
          (with-output-to-string
            (lambda ()
              (output (map (lambda (stx)
                             (eval stx namespace))
                           (syntax->datum syntax)))))))]))


(define (path->absolute-path path)
  (if (relative-path? path)
    (build-path (current-directory) path)
    (build-path path)))


(define (current-file-or-directory-attributes path)
  (values
    (file-or-directory-permissions path 'bits)
    (file-or-directory-owner path)
    (file-or-directory-group path)))


(define (file-or-directory-owner path [new-owner #f])
  (if new-owner
    (let ([passwd (getpwnam new-owner)])
      (unless passwd (provee:raise/system-call 'getpwnam))
      (unless (= (chown path (passwd-uid passwd) -1) 0)
        (provee:raise/system-call 'chown))
      #t)
    (let* ([stat-buffer (cast (malloc _statstruct) _pointer
                              _statstruct-pointer)]
           [stat-result (stat path stat-buffer)])
      (when (< stat-result 0)
        (provee:raise/system-call 'stat))
      (let ([passwd (getpwuid (statstruct-uid stat-buffer))])
        (unless passwd
          (provee:raise/system-call 'getpwuid))
        (passwd-name passwd)))))


(define (file-or-directory-group path [new-group #f])
    (if new-group
      (let ([group (getgrnam new-group)])
        (unless group (provee:raise/system-call 'getgrnam))
        (unless (= (chown path -1 (group-gid group)) 0)
          (provee:raise/system-call 'chown))
        #t)
      (let* ([stat-buffer (cast (malloc _statstruct) _pointer
                                _statstruct-pointer)]
             [stat-result (stat path stat-buffer)])
        (when (< stat-result 0)
          (provee:raise/system-call 'stat))
        (let ([group (getgrgid (statstruct-gid stat-buffer))])
          (unless group
            (provee:raise/system-call 'getgrgid))
          (group-name group)))))
