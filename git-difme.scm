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

;;;; configuration
(define (load-config path)
  "load configuration file from PATH.

errors out if PATH does not exists."
  (if (file-exists? path)
      (load path)
      (error "config not found at" path)))

(define (get-config-path)
  "return configuration file path as a string."
  (string-append (getenv "HOME") "/.config/git-difme/config"))

(define (get-difme-repos)
  "return difme repos returned by `difme-repos` function.

`difme-repos` is defined in the configuration file; this function
loads the configuration file and then evals the `difme-repos`
function."
  (let ((path (get-config-path)))
    (load-config path)
    (eval '(difme-repos) (interaction-environment))))

;;;; git interfaces
(define (staged-files repo)
  "return list of staged files in REPO."
  (let ((cmd "git diff --name-only --cached"))
    (with-directory-excursion repo
      (difme-exec cmd))))

(define (difme-status repo)
  "do `git status` on the REPO and return output as a list.

if the output from `git status` is:

 M .asoundrc
 D .config/i3/config

the returned list will be in the following format:

     ((\"M\" . \".asoundrc\") (\"D\" . \".config/i3/config\"))

if there is no output from `git status`, then an empty list will be
returned."
  (let ((cmd "git status --porcelain"))
    (define (process line)
      (let* ((parts (string-split line #\space))
             (type (car parts))
             (file (cadr parts)))
        (cons type file)))
    (with-directory-excursion repo
      (map process (difme-exec cmd)))))

(define (difme-stage repo file)
"stage FILE in REPO."
  (let ((cmd (string-append "git add " file)))
    (with-directory-excursion repo
      (difme-exec cmd))))

(define (difme-commit repo msg)
"do `git commit` on REPO.

the commit message will be MSG followed by a list of files staged for
this commit.

if files `foo.org`, `bar.scm` and `frob.el` are staged for the commit,
the commit message will be in the following format:

    MSG

    files:
    - foo.org
    - bar.scm
    - frob.el"
  (let* ((msg (string-append
               msg "\n\nfiles:\n - "
               (string-join (staged-files repo) "\n - ")))
        (cmd (string-append "git commit -m '" msg "'")))
    (with-directory-excursion repo
      (difme-exec cmd))))

(define (difme-push repo)
  "do `git push` REPO to its default upstream remote."
  (let ((cmd "git push"))
    (with-directory-excursion repo
      (difme-exec cmd))))

;;;; main
(define (main srcs)
  srcs)
