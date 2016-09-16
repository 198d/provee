#lang racket
(provide getpwnam
         getgrnam
         getpwuid
         getgrgid
         stat
         chown
         group-name
         group-gid
         passwd-name
         passwd-uid
         passwd-gid
         passwd-dir
         _statstruct
         _statstruct-pointer
         timespec-sec
         statstruct-uid
         statstruct-gid
         statstruct-ctim)


(require ffi/unsafe
         (for-syntax syntax/parse))


(define LIB (ffi-lib #f))


(define-syntax (for-os stx)
  (syntax-parse stx
    [(_ os-type clause ...)
     (if (equal? `',(system-type) (syntax->datum #'os-type))
       #'(begin clause ...)
       #'(void))]))


(for-os 'macosx
  (define-cstruct _passwd ([name _string] [passwd _string] [uid _int]
                           [gid _int] [change _long] [class _string]
                           [gecos _string] [dir _string] [shell _string]
                           [expire _long])))


(for-os 'unix
  (define-cstruct _passwd ([name _string] [passwd _string] [uid _uint]
                           [gid _uint] [gecos _string] [dir _string]
                           [shell _string])))


(define-cstruct _group ([name _string] [passwd _string] [gid _uint]
                        [mem _pointer]))


(define-cstruct _timespec ([sec _long] [nsec _long]))


(for-os 'macosx
  (define-cstruct _statstruct([dev _int] [mode _ushort] [nlink _ushort]
                              [ino _ulong] [uid _uint] [gid _uint]
                              [rdev _int] [atim _timespec]
                              [mtim _timespec] [ctim _timespec]
                              [birthtimespec _timespec] [size _long]
                              [blocks _long] [blksize _int]
                              [flags _uint] [gen _uint] [lspare _int]
                              [st (_array _long 2)])))

(for-os 'unix
  (define-cstruct _statstruct([dev _long] [ino _long] [nlink _long]
                              [mode _uint] [uid _uint] [gid _uint]
                              [__pad0 _int] [rdev _ulong] [size _long]
                              [blksize _long] [blocks _long] [atim _timespec]
                              [mtim _timespec] [ctim _timespec]
                              [__glibc_reserved (_array _long 3)])))


(define getpwnam (get-ffi-obj "getpwnam" LIB
                              (_fun #:save-errno 'posix
                                    _string -> _passwd-pointer/null)))


(define getpwuid (get-ffi-obj "getpwuid" LIB
                              (_fun #:save-errno 'posix
                                    _int -> _passwd-pointer/null)))


(define getgrnam (get-ffi-obj "getgrnam" LIB
                              (_fun #:save-errno 'posix
                                    _string -> _group-pointer/null)))


(define getgrgid (get-ffi-obj "getgrgid" LIB
                              (_fun #:save-errno 'posix
                                    _int -> _group-pointer/null)))


(define chown (get-ffi-obj "chown" LIB
                           (_fun #:save-errno 'posix
                                 _string _int _int -> _int)))


(for-os 'macosx
  (define stat (get-ffi-obj "stat64" LIB
                            (_fun #:save-errno 'posix
                                  _string _statstruct-pointer -> _int))))

(for-os 'unix
  (define __xstat (get-ffi-obj
                    "__xstat" LIB
                    (_fun #:save-errno 'posix
                          _int _string _statstruct-pointer -> _int)))
  (define stat (curry __xstat 1)))
