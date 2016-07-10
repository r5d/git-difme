;; coding: utf-8
;; license: gnu gpl version 3 or higher.
;; copyright 2016 rsiddharth <s@ricketyspace.net>

(define-module (git-difme)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:export (main))

;;;; utils

;;; following macro from (guix build utils) module.
;;; copyright 2012 Ludovic Courtès <ludo@gnu.org>
;;; commit b0e0d0e99f

(define-syntax-rule (with-directory-excursion dir body ...)
  "run BODY with DIR as the process's current directory."
  (let ((init (getcwd)))
    (dynamic-wind
      (lambda () (chdir dir))
      (lambda () body ...)
      (lambda () (chdir init)))))

(define (difme-exec cmd)
  "execute CMD and return output as a list of strings."
  (let* ((port (open-input-pipe cmd))
         (out (read-string port))
         (out-lst (map string-trim-both
                       (delete "" (string-split out #\newline)))))
    (close-pipe port)
    out-lst))

;;;; main
(define (main srcs)
  srcs)
