;; Install additional packages in prelude
;; This file is part of prelude-packages.el customized

;; Add marmalade repo for following packages: geben
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar prelude-personal-packages
  '(ecb php-mode php+-mode auto-complete emms flymake-php
        geben)
  "A list of additional packages to ensure are installed at launch.")

(defun prelude-personal-packages-installed-p ()
  (loop for p in prelude-personal-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun prelude-install-personal-packages ()
  (unless (prelude-personal-packages-installed-p)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p prelude-personal-packages)
      (unless (package-installed-p p)
        (package-install p)))))

(prelude-install-personal-packages)
