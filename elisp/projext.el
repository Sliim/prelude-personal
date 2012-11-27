;;; projext.el --- Extension for project managment utilities projectile/php-project

;; Author: Sliim
;; Version: 0.1
;; Keywords: projectile php-project project

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
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

;;; Commentary:
;;
;; Projext provide an extension for projectile and php-project utilities
;; Some features of projext
;; - Open/Close a php-project
;; - Save project desktop at a time
;;
;; TODO
;; - Custom desktop file
;;
;;; Code:

;; requires
(require 'php-project)
(require 'projectile)

(defgroup projext nil
  "Extension for project managment utilities projectile/php-project"
  :group 'projectile
  :group 'php-project)

(defcustom projext-directory ".emacs/"
  "Directory where stored TAGS, desktop, snippets files"
  :group 'projext
  :type 'string)

(defcustom projext-config-file "projext-config.el"
  "Specific configuration file for a project"
  :group 'projext
  :type 'string)

(defvar projext-current-project nil
  "Current project.")

(defun projext-open-project ()
  "Function that open project, load snippets, visit tags table and read project desktop if exists"
  (interactive)
  (let ((project (php-project-ask-for-project "Project: ")))
    (projext-close-current-project)
    (php-project-dired-directory project)
    (setq projext-emacs-dir (concat (php-project-directory project) projext-directory))
    (when (and (/= (length (php-project-tags-file project)) 0)
               (file-exists-p (php-project-tags-file project)))
      (message "Loading project's tags table..")
      (tags-reset-tags-tables)
      (visit-tags-table (php-project-tags-file project)))
    (when (file-exists-p projext-emacs-dir)
      ;; TODO use custom variable projext-directory
      (setq desktop-path '(concat (php-project-directory project) ".emacs/"))
      (setq desktop-dirname projext-emacs-dir)
      (setq projext-snippets-dir (concat projext-emacs-dir "snippets/"))

      (when (file-exists-p projext-snippets-dir)
        (message "Loading project's snippets..")
        (add-to-list 'yas/root-directory projext-snippets-dir)
        (mapc 'yas/load-directory yas/root-directory))
      (when (file-exists-p (concat projext-emacs-dir ".emacs.desktop"))
        (message "Loading project's desktop..")
        (desktop-read))
      (when (file-exists-p (concat projext-emacs-dir projext-config-file))
        (message "Loading project's configuration..")
        (load-file (concat projext-emacs-dir projext-config-file))))
    (setq projext-current-project project)
    (setq tags-completion-table nil)
    (projext-set-projectile-tags-command)
    (message (concat "Project " (php-project-nickname projext-current-project) " opened."))))

(defun projext-show-current-project ()
  "Show the current project"
  (interactive)
  (if projext-current-project
      (message (php-project-nickname projext-current-project))
    (message "none")))

(defun projext-close-current-project ()
  "Close current project"
  (interactive)
  (when projext-current-project
    (when (y-or-n-p (concat "Save desktop for current project " (php-project-nickname projext-current-project) " "))
      (projext-save-project-desktop))
    (desktop-clear)
    (projext-remove-project-desktop-lock-file)
    (message (concat "Project " (php-project-nickname projext-current-project) " closed."))
    (setq projext-current-project nil)
    (projext-set-projectile-tags-command)))

(defun projext-save-project-desktop ()
  "Function that save current desktop in .emacs's project dir"
  (interactive)
  (if projext-current-project
      (when (file-exists-p (concat (php-project-directory projext-current-project) projext-directory))
        (desktop-save (concat (php-project-directory projext-current-project) projext-directory))
        (message "Desktop saved."))
    (message "No project opened..")))

(defun projext-clear-project-desktop ()
  "Overload desktop-clear to open current project directory when clearing desktop"
  (interactive)
  (desktop-clear)
  (when projext-current-project
    (php-project-dired-directory projext-current-project)))

(defun projext-regenerate-tags ()
  "Regenerate project's tags table"
  (interactive)
  (projext-set-projectile-tags-command)
  (if (and projext-current-project (/= (length (php-project-tags-file projext-current-project)) 0))
      (progn
        (shell-command (format projectile-tags-command (php-project-directory projext-current-project)))
        (visit-tags-table (php-project-tags-file projext-current-project)))
    (projectile-regenerate-tags)))

(defun projext-clean-project-tags ()
  "Clear tags table and remove tags file"
  (interactive)
  (when projext-current-project
    (tags-reset-tags-tables)
    (when (/= (length (php-project-tags-file projext-current-project)) 0)
      (delete-file (php-project-tags-file projext-current-project)))))

(defun projext-clean-project-desktop ()
  "Clear desktop and remove files"
  (interactive)
  (when projext-current-project
    (setq projext-emacs-dir (concat (php-project-directory projext-current-project) projext-directory))
    (projext-clear-project-desktop)
    (when (file-exists-p (concat projext-emacs-dir ".emacs.desktop"))
      (delete-file (concat projext-emacs-dir ".emacs.desktop")))
    (when (file-exists-p (concat projext-emacs-dir ".emacs.desktop.lock"))
      (delete-file (concat projext-emacs-dir ".emacs.desktop.lock")))))

(defun projext-clean-project ()
  "Remove project's TAGS and desktop files"
  (interactive)
  (projext-clean-project-desktop)
  (projext-clean-project-tags))

(defun projext-remove-project-desktop-lock-file ()
  "Remove desktop lock file"
  (when projext-current-project
    (setq projext-desktop-lock-file (concat (php-project-directory projext-current-project) projext-directory ".emacs.desktop.lock"))
    (when (file-exists-p projext-desktop-lock-file)
      (delete-file (projext-desktop-lock-file))
      (message "Desktop lock file removed."))))

(defun projext-set-projectile-tags-command ()
  "Set projectile-tags-command custom variable"
  (setq projectile-tags-command "ctags-exuberant -Re \
    --languages=PHP \
    --exclude=\"\.git\" \
    --totals=yes \
    --tag-relative=yes \
    --PHP-kinds=-v \
    --regex-PHP='/abstract class ([^ ]*)/\1/c/' \
    --regex-PHP='/trait ([^ ]*)/\1/c/' \
    --regex-PHP='/interface ([^ ]*)/\1/c/' \
    --regex-PHP='/(public |final |static |abstract |protected |private )+function ([^ (]*)/\2/f/' \
    --regex-PHP='/const ([^ ]*)/\1/d/'")

  (when projext-current-project
    (when (/= (length (php-project-tags-file projext-current-project)) 0)
      (setq projectile-tags-command (concat projectile-tags-command " -o " (php-project-tags-file projext-current-project))))))

(provide 'projext)

;;; projext.el ends here
