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
;; Some features of projext:
;; - Open/Close a php-project
;; - Save project desktop at a time
;;
;;; Code:

;; requires
(require 'php-project)
(require 'projectile)

(defgroup projext nil
  "Extension for project managment utilities projectile/php-project"
  :group 'projectile
  :group 'php-project)

(defcustom projext-directory ".project/"
  "Directory where stored TAGS, desktop, snippets files"
  :group 'projext
  :type 'string)

(defcustom projext-config-file "projext-config.el"
  "Specific configuration file for a project"
  :group 'projext
  :type 'string)

(defcustom projext-desktop-file "emacs.desktop"
  "Project's desktop filename"
  :group 'projext
  :type 'string)

(defvar projext-current-project nil
  "Current project.")

(defun projext-open-project ()
  "Function that open project, load snippets, visit tags table and read project desktop if exists"
  (interactive)
  (let ((project (php-project-ask-for-project "Project: ")))
    (let ((p-emacs-dir (concat (php-project-directory project) projext-directory))
          (p-tags-file (php-project-tags-file project)))
      (projext-close-current-project)
      (php-project-dired-directory project)
      (setq projext-current-project project)
      (when (and (/= (length p-tags-file) 0)
                 (file-exists-p p-tags-file))
        (message "Loading project's tags table..")
        (tags-reset-tags-tables)
        (visit-tags-table p-tags-file))
      (when (file-exists-p p-emacs-dir)
        (setq desktop-path (list p-emacs-dir)
              desktop-dirname p-emacs-dir
              desktop-base-file-name projext-desktop-file
              desktop-base-lock-name (concat projext-desktop-file ".lock")
              tags-completion-table nil)
        (let ((p-snippets-dir (concat p-emacs-dir "snippets/"))
              (p-desktop-file (concat p-emacs-dir projext-desktop-file))
              (p-config-file (concat p-emacs-dir projext-config-file)))
          (when (file-exists-p p-snippets-dir)
            (message "Loading project's snippets..")
            (yas-load-directory p-snippets-dir))
          (when (file-exists-p p-desktop-file)
            (message "Loading project's desktop..")
            (desktop-read))
          (when (file-exists-p p-config-file)
            (message "Loading project's configuration..")
            (load-file p-config-file))))
      (message (concat "Project " (php-project-nickname projext-current-project) " opened.")))))

(defun projext-show-current-project ()
  "Show the current project"
  (interactive)
  (if projext-current-project
      (message (php-project-nickname projext-current-project))
    (message "none")))

(defun projext-close-current-project ()
  "Close current project"
  (interactive)
  (let ((p-name (php-project-nickname projext-current-project)))
    (when projext-current-project
      (when (y-or-n-p (concat "Save desktop for current project " p-name " "))
        (projext-save-project-desktop))
      (projext-clear-project-desktop)
      (projext-remove-project-desktop-lock-file)
      (message (concat "Project " p-name " closed."))
      (setq projext-current-project nil))))

(defun projext-save-project-desktop ()
  "Function that save current desktop in project's directory"
  (interactive)
  (if projext-current-project
      (progn
        (let ((p-directory (concat (php-project-directory projext-current-project) projext-directory)))
          (when (file-exists-p p-directory)
            (desktop-save p-directory)
            (message "Desktop saved."))))
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
  (projext-clean-project-tags)
  (projext-set-projectile-tags-command)
  (let ((p-tags-file (php-project-tags-file projext-current-project)))
    (if (and projext-current-project (/= (length p-tags-file) 0))
        (progn
          (shell-command (format projectile-tags-command (php-project-directory projext-current-project)))
          (visit-tags-table p-tags-file))
      (projectile-regenerate-tags))))

(defun projext-clean-project-tags ()
  "Clear tags table and remove tags file"
  (interactive)
  (tags-reset-tags-tables)
  (when projext-current-project
    (let ((p-tags-file (php-project-tags-file projext-current-project)))
      (when (and
             (/= (length p-tags-file) 0)
             (file-exists-p p-tags-file))
        (delete-file p-tags-file))))
  (when  (and (setq project-root (projectile-project-root)) (file-exists-p (concat project-root "TAGS")))
    (delete-file (concat project-root "TAGS"))))

(defun projext-clean-project-desktop ()
  "Clear desktop and remove files"
  (interactive)
  (when projext-current-project
    (let ((p-desktop-file (concat (concat (php-project-directory projext-current-project) projext-directory) projext-desktop-file)))
      (projext-clear-project-desktop)
      (when (file-exists-p p-desktop-file)
        (delete-file p-desktop-file))
      (projext-remove-project-desktop-lock-file))))

(defun projext-clean-project ()
  "Remove project's TAGS and desktop files"
  (interactive)
  (projext-clean-project-desktop)
  (projext-clean-project-tags))

(defun projext-remove-project-desktop-lock-file ()
  "Remove desktop lock file"
  (when projext-current-project
    (let ((p-desktop-lock (concat (php-project-directory projext-current-project) projext-directory projext-desktop-file ".lock")))
      (when (file-exists-p p-desktop-lock)
        (delete-file p-desktop-lock)))))

(defun projext-set-projectile-tags-command ()
  "Set projectile-tags-command custom variable"
  (setq p-base-command "ctags-exuberant -Re \
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

  (when (and projext-current-project (/= (length (php-project-tags-file projext-current-project)) 0))
    (setq p-base-command (concat p-base-command " -o " (php-project-tags-file projext-current-project))))
  (setq projectile-tags-command (concat p-base-command " %s")))

(provide 'projext)

;;; projext.el ends here
