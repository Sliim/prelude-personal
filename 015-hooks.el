;;; 015-hooks.el --- Emacs Prelude: Personal emacs hook's definitions
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: prelude personal

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal hook's definitions

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

(defun php-mode-personal-hook ()
  "Function to be called when entering into `php-mode`."
  (interactive)
  (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
    (make-local-variable 'ac-sources)
    (setq ac-sources '(ac-source-words-in-same-mode-buffers
                       ac-source-dictionary))
    (when (and (require 'auto-complete-etags nil t) tags-table-list)
      (add-to-list 'ac-sources 'ac-source-etags))
    (auto-complete-mode t))
  (flymake-php-load)
  (turn-on-eldoc-mode)
  (c-set-offset 'case-label '+)
  (define-key php-mode-map (kbd "RET") 'newline-and-indent))

(defun python-mode-personal-hook ()
  "Function to be called when entering into `python-mode`."
  (interactive)
  (jedi:setup)
  (eldoc-mode 1))


;;; 015-hooks.el ends here
