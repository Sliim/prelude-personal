;;; skbd.el --- This is a minor mode to manage my personal kbd map
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: prelude personal keybindings

;; This file is not part of GNU Emacs.

;;; Commentary:

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
(require 'helm)
(require 'helm-git)
(require 'helm-etags+)
(require 'projectile)
(require 'projext)
(require 'magit)
(require 'etags-select)
(require 'emms)

(defgroup skbd nil
  "Sliim personal keybindings"
  :group 'prelude
  :group 'projext
  :group 'helm
  :group 'emms
  :group 'etags-select-mode)

(defvar skbd-keymap-prefix (kbd "C-M-<")
  "Skbd keymap prefix.")

(defvar skbd-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "t") 'etags-select-find-tag)
      (define-key prefix-map (kbd "h g") 'helm-git-find-files)
      (define-key prefix-map (kbd "h t") 'helm-etags+-select)
      (define-key prefix-map (kbd "h p") 'helm-prelude)
      (define-key prefix-map (kbd "h f") 'helm-find-files)
      (when (require 'helm-ls-hg nil t)
        (define-key prefix-map (kbd "h h") 'helm-hg-find-files-in-project))
      (define-key prefix-map (kbd "p o") 'projext-open-project)
      (define-key prefix-map (kbd "p i") 'projext-show-current-project)
      (define-key prefix-map (kbd "p x") 'projext-close-current-project)
      (define-key prefix-map (kbd "p s") 'projext-save-project-desktop)
      (define-key prefix-map (kbd "p t") 'projext-regenerate-tags)
      (define-key prefix-map (kbd "p c t") 'projext-clean-project-tags)
      (define-key prefix-map (kbd "p c d") 'projext-clean-project-desktop)
      (define-key prefix-map (kbd "p c a") 'projext-clean-project)
      (define-key prefix-map (kbd "e a") 'emms-add-directory-tree)
      (define-key prefix-map (kbd "e n") 'emms-next)
      (define-key prefix-map (kbd "e b") 'emms-previous)
      (define-key prefix-map (kbd "e p") 'emms-pause)
      (define-key prefix-map (kbd "e h") 'helm-emms)
      (define-key prefix-map (kbd "e l") 'emms-browser)
      (define-key prefix-map (kbd "f") 'projectile-find-file)
      (define-key prefix-map (kbd "g") 'magit-status)
      (define-key prefix-map (kbd "m") 'monky-status)
      (define-key prefix-map (kbd "w") 'whitespace-mode)
      (define-key prefix-map (kbd "i") 'iedit-mode)

      (define-key map skbd-keymap-prefix prefix-map))
    map)
  "Keymap for Skbd mode.")

;;;###autoload
(define-globalized-minor-mode skbd-global-mode
  skbd-mode
  skbd-mode 1)

;;;###autoload
(define-minor-mode skbd-mode
  "Minor mode for Sliim's keybindings map."
  :lighter " Skbd"
  :keymap skbd-mode-map
  :group 'skbd)

(provide 'skbd)

;;; skbd.el ends here
