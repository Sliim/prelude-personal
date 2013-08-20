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

(defun json-pretty-format ()
  "Print json string into an human readable format.
This function run external shell command `python -m json.tool` on current region."
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "python -m json.tool"))

(defun nxml-pretty-format()
  "Pretty print XML format. Requires xmllint into path."
  (interactive)
  (save-excursion
    (shell-command-on-region (region-beginning) (region-end) "xmllint --format -" (buffer-name) t)
    (nxml-mode)
    (indent-region begin end)))

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
  (browse-url-of-buffer (markdown-standalone markdown-output-buffer-name)))

;; Eshell utilities
(defun eshell/clear ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/magit ()
  "Open magit in current repository."
  (interactive)
  (magit-status (eshell/pwd)))

(defun git-current-branch (pwd)
  "Return current git branch as a string in current directory `PWD`."
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat " ["
                          (if (> (length git-output) 0)
                              (concat (substring git-output 0 -1) (git-changes pwd))
                            "(no branch)")
                          "]") 'face `(:foreground "green")))))

(defun git-unpushed-commits (pwd)
  "Return number of unpushed commits in repository `PWD`."
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git log @{u}.. --oneline 2>/dev/null | wc -l"))))
      (let ((out (substring git-output 0 -1)))
        (when (not (string= out "0"))
          (propertize (concat " [" out "]")
                      'face `(:foreground "red")))))))

(defun git-changes (pwd)
  "Get modified files in repository `PWD`."
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string
                       (concat "cd " pwd " && git status --short | wc -l"))))
      (let ((out (substring git-output 0 -1)))
        (when (not (string= out "0")) " ⚡")))))

(defun tmux-kbd-init-client()
  "Remap some keys for Tmux session."
  (interactive)
  (define-key input-decode-map "\M-[1;2A" [S-up])
  (define-key input-decode-map "\M-[1;2B" [S-down])
  (define-key input-decode-map "\M-[1;2C" [S-right])
  (define-key input-decode-map "\M-[1;2D" [S-left])
  (define-key input-decode-map "\M-[1;5A" [C-up])
  (define-key input-decode-map "\M-[1;5B" [C-down])
  (define-key input-decode-map "\M-[1;5C" [C-right])
  (define-key input-decode-map "\M-[1;5D" [C-left])
  (define-key input-decode-map "\M-[1~" [home])
  (define-key input-decode-map "\M-[4~" [end]))

(defun helm-buffers-right-side ()
  "Special helm settings to list buffers in right side."
  (interactive)
  (let ((initial-helm-split-window-default-side helm-split-window-default-side))
    (setq helm-split-window-default-side (quote right))
    (helm-buffers-list)
    (setq helm-split-window-default-side initial-helm-split-window-default-side)))

(defun set-cmd-to-kbd (command keybinding)
  "Bind COMMAND to a KEYBINDING quickly."
  (interactive "sCommand: \nsKeybinding: ")
  (setq cmd command)
  (define-key skbd-mode-map (kbd keybinding) `(lambda ()
                                         (interactive)
                                         (let ((default-directory ,default-directory))
                                           (compile ,cmd)))))

;;; 010-functions.el ends here
