(include "~~spheres/prelude#.scm")
(%include sake: utils#)

(define modules '(srfi-1
                  ;srfi-2
                  ;srfi-5
                  ;srfi-11
                  ;srfi-13
                  srfi-14
                  ;srfi-16
                  ;srfi-19
                  srfi-25
                  ;srfi-26
                  srfi-28
                  ;srfi-31
                  ;srfi-34
                  ;srfi-35
                  srfi-37
                  srfi-38
                  ;srfi-40
                  ;srfi-42
                  ;srfi-43
                  ;srfi-45
                  srfi-47
                  ;srfi-51
                  ;srfi-54
                  ;srfi-61
                  ;srfi-64
                  srfi-69
                  srfi-95))

(define-task clean ()
  (sake:default-clean))

(define-task compile ()
  (sake:parallel-for-each
   (lambda (m)
     (sake:compile-c-to-o (sake:compile-to-c m))
     (sake:compile-c-to-o (sake:compile-to-c
                           m
                           version: '(debug)
                           compiler-options: '(debug))))
   modules
   max-thread-number: 8))

(define-task install ()
  (for-each (lambda (m)
              (sake:install-compiled-module m)
              (sake:install-compiled-module m version: '(debug)))
            modules)
  (sake:install-system-sphere))

(define-task uninstall ()
  (sake:uninstall-system-sphere))

(define-task all (compile install)
  'all)
