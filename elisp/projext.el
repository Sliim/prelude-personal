;;; projext.el --- Extension for project managment utilities projectile/project-persist
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: prelude personal projectile project-persist project

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Projext provide an extension for projectile and project-persist utilities

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

;; requires
(require 'project-persist)
(require 'projectile)

(defgroup projext nil
  "Extension for project managment utilities projectile/project-persist"
  :group 'projectile
  :group 'project-persist)

(defcustom projext-directory ".project/"
  "Directory where stored TAGS, desktop, snippets files."
  :group 'projext
  :type 'string)

(defcustom projext-config-file "projext-config.el"
  "Specific configuration file for a project."
  :group 'projext
  :type 'string)

(defcustom projext-desktop-file "emacs.desktop"
  "Project's desktop filename."
  :group 'projext
  :type 'string)

(defcustom projext-tags-file "TAGS"
  "Project's tags file."
  :group 'projext
  :type 'string)

(defun projext-init ()
  "Projext initialization."
  (require 'project-persist)
  (project-persist-mode t)
  (projext-set-projectile-tags-command)
  (add-hook 'kill-emacs-hook 'projext-close-if-opened)
  (add-hook 'project-persist-before-load-hook 'projext-close-if-opened)
  (add-hook 'project-persist-after-load-hook 'projext-open-project-hook)
  (add-hook 'project-persist-before-close-hook 'projext-close-current-project-hook)
  (add-hook 'project-persist-after-save-hook 'projext-save-project-desktop))

(defun projext-show-current-project ()
  "Show the current project."
  (interactive)
  (if (pp/has-open-project)
      (message project-persist-current-project-name)
    (message "none")))

(defun projext-save-project-desktop ()
  "Function that save current desktop in project's directory."
  (interactive)
  (if (pp/has-open-project)
      (progn
        (let ((p-directory (projext-get-projext-directory)))
          (when (file-exists-p p-directory)
            (desktop-save p-directory)
            (message "Desktop saved."))))
    (message "No project opened..")))

(defun projext-clear-project-desktop ()
  "Overload `desktop-clear` to open current project directory when clearing desktop."
  (interactive)
  (desktop-clear)
  (when (pp/has-open-project)
    (dired project-persist-current-project-root-dir)))

(defun projext-regenerate-tags ()
  "Regenerate project's tags table."
  (interactive)
  (projext-clean-project-tags)
  (projext-set-projectile-tags-command)
  (let ((p-tags-file (concat (projext-get-projext-directory) projext-tags-file)))
    (if (and (pp/has-open-project))
        (progn
          (shell-command (format projectile-tags-command project-persist-current-project-root-dir))
          (visit-tags-table p-tags-file))
      (projectile-regenerate-tags))))

(defun projext-clean-project-tags ()
  "Clear tags table and remove tags file."
  (interactive)
  (tags-reset-tags-tables)
  (when (pp/has-open-project)
    (let ((p-tags-file (concat (projext-get-projext-directory) projext-tags-file)))
      (when (file-exists-p p-tags-file)
        (delete-file p-tags-file))))
  (when (file-exists-p (concat (projectile-project-root) "TAGS"))
    (delete-file (concat (projectile-project-root) "TAGS"))))

(defun projext-clean-project-desktop ()
  "Clear desktop and remove desktop files."
  (interactive)
  (when (pp/has-open-project)
    (let ((p-desktop-file (concat (projext-get-projext-directory) projext-desktop-file)))
      (projext-clear-project-desktop)
      (when (file-exists-p p-desktop-file)
        (delete-file p-desktop-file))
      (projext-remove-project-desktop-lock-file))))

(defun projext-clean-project ()
  "Remove project's TAGS and desktop files."
  (interactive)
  (projext-clean-project-desktop)
  (projext-clean-project-tags))

(defun projext-get-projext-directory ()
  "Return project subdirectory where are stored TAGS file, desktop etc.."
  (concat project-persist-current-project-root-dir "/" projext-directory))

(defun projext-remove-project-desktop-lock-file ()
  "Remove desktop lock file."
  (when (pp/has-open-project)
    (let ((p-desktop-lock (concat (projext-get-projext-directory) projext-desktop-file ".lock")))
      (when (file-exists-p p-desktop-lock)
        (delete-file p-desktop-lock)))))

(defun projext-close-if-opened ()
  "Close current project if opened."
  (when (pp/has-open-project)
    (project-persist-close)))

(defun projext-set-projectile-tags-command ()
  "Set projectile-tags-command custom variable."
  (setq p-base-command "ctags-exuberant -Re \
    --languages=PHP,Python,Ruby,JavaScript,C,sh \
    --exclude=\"\.git\" \
    --totals=yes \
    --tag-relative=yes \
    --PHP-kinds=-v \
    --regex-PHP='/abstract class ([^ ]*)/\1/c/' \
    --regex-PHP='/trait ([^ ]*)/\1/c/' \
    --regex-PHP='/interface ([^ ]*)/\1/c/' \
    --regex-PHP='/(public |final |static |abstract |protected |private )+function ([^ (]*)/\2/f/' \
    --regex-PHP='/const ([^ ]*)/\1/d/'")

  (when (pp/has-open-project)
    (setq p-base-command (concat p-base-command " -o " (projext-get-projext-directory) projext-tags-file)))
  (setq projectile-tags-command (concat p-base-command " %s")))

(defun projext-open-project-hook ()
  "Hook executed when open project: Load snippets, visit tags table and read project desktop if exists."
  (let ((p-emacs-dir (projext-get-projext-directory)))
    (dired project-persist-current-project-root-dir)
    (if (file-exists-p p-emacs-dir)
        (progn
          (setq desktop-path (list p-emacs-dir)
                desktop-dirname p-emacs-dir
                desktop-base-file-name projext-desktop-file
                desktop-base-lock-name (concat projext-desktop-file ".lock")
                tags-completion-table nil)
          (let ((p-snippets-dir (concat p-emacs-dir "snippets/"))
                (p-desktop-file (concat p-emacs-dir projext-desktop-file))
                (p-config-file (concat p-emacs-dir projext-config-file))
                (p-tags-file (concat p-emacs-dir projext-tags-file)))
            (when (file-exists-p p-tags-file)
              (message "Loading project's tags table..")
              (tags-reset-tags-tables)
              (visit-tags-table p-tags-file))
            (when (file-exists-p p-snippets-dir)
              (message "Loading project's snippets..")
              (yas-load-directory p-snippets-dir))
            (when (file-exists-p p-desktop-file)
              (message "Loading project's desktop..")
              (desktop-read))
            (when (file-exists-p p-config-file)
              (message "Loading project's configuration..")
              (load-file p-config-file))))
      (mkdir p-emacs-dir))
    (message (concat "Project " project-persist-current-project-name " opened."))))

(defun projext-close-current-project-hook ()
  "Hook executed before closing current project."
  (projext-clear-project-desktop)
  (projext-remove-project-desktop-lock-file)
  (message (concat "Project " project-persist-current-project-name " closed.")))

(provide 'projext)

;;; projext.el ends here
