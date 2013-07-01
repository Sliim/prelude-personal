;;; 000-packages.el --- Emacs Prelude: Personal emacs packages installation.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: prelude personal

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Install additional packages in prelude
;; This file come from prelude-packages.el customized

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Add marmalade repo for following packages: geben, php-extras
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar prelude-personal-packages
  '(ecb php-mode auto-complete emms flymake-php iedit helm-git monky markdown-mode ir-black-theme
        python-mode flymake-python-pyflakes jedi epc deferred nose pep8 rbenv
        js-comint flymake-jslint js2-mode flymake-cursor
        project-persist helm-project-persist
        geben php-extras etags-select popwin shell-pop perspective
        puppet-mode)
  "A list of additional packages to ensure are installed at launch.")

(defun prelude-personal-packages-installed-p ()
  (loop for p in prelude-personal-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun prelude-install-personal-packages ()
  "Install defined packages."
  (unless (prelude-personal-packages-installed-p)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p prelude-personal-packages)
      (unless (package-installed-p p)
        (package-install p)))))

(prelude-install-personal-packages)

;;; 000-packages.el ends here
