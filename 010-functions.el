;;; 010-functions.el --- Emacs Prelude: Personal emacs functions declaration
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: prelude personal

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal functions

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

(defun ecb-refresh ()
  "Refresh ecb window (directories, methods, history)."
  (interactive)
  (ecb-update-directories-buffer)
  (ecb-clear-history)
  (ecb-rebuild-methods-buffer))

(defun json-to-human-format ()
  "Print json string into an human readable format.
This function run external shell command `python -m json.tool` on current region."
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "python -m json.tool"))

(defun eshell/clear ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun nose-toggle-python-version ()
  "Toggle python version for nosetests."
  (interactive)
  (if (string= "nosetests" nose-global-name)
      (setq nose-global-name "nosetests-3.2")
    (setq nose-global-name "nosetests"))
  (message  (concat "Nose: " nose-global-name)))

(defun markdown-preview-with-hf (&optional output-buffer-name)
  "Run `markdown' on the current buffer and preview the output 'OUTPUT-BUFFER-NAME' in a browser."
  (interactive)
  (browse-url-of-buffer (markdown-standalone markdown-output-buffer-name))


;;; 010-functions.el ends here
  )
