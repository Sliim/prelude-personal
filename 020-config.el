;;; 020-config.el --- Emacs Prelude: Personal configuration.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: prelude personal

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Personal configuration

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

;;Load ir-black theme
(load-theme 'ir-black t)

;;Global config
(setq-default tab-width 4)
(setq confirm-kill-emacs 'yes-or-no-p
      require-final-newline t
      c-basic-offset 4
      scroll-bar-mode nil
      scroll-conservatively 10000
      scroll-step 1
      delete-trailing-lines t
      global-auto-revert-mode t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount (quote (2 ((shift) . 2) ((control)))))

;; Print config
(setq ps-font-size 8
      ps-header-font-size 9
      ps-header-title-font-size 10
      ps-line-number t
      ps-line-number-font-size 8
      ps-line-number-step 1
      ps-print-color-p (quote black-white))

;; Prelude config
(setq prelude-flyspell nil
      prelude-guru t
      prelude-whitespace nil)

;; Prog-mode
(add-hook 'prelude-prog-mode-hook 'prog-mode-personal-hook)

;; Auto mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; Add vendor/ dir to load-path
(add-to-list 'load-path "~/.emacs.d/personal/elisp")
(add-to-list 'load-path "~/.emacs.d/personal/vendor")

;; Projectile
(add-to-list 'projectile-globally-ignored-directories ".project")
(setq projectile-globally-ignored-files '())

;; Sr-Speedbar
(require 'sr-speedbar)
(add-hook 'speedbar-mode-hook '(lambda () (hl-line-mode 1)))
(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-directory-button-trim-method 'trim
      speedbar-use-images nil
      speedbar-indentation-width 2
      speedbar-use-imenu-flag t
      sr-speedbar-width 35
      sr-speedbar-width-x 35
      sr-speedbar-auto-refresh t
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil)

;; Grep
(grep-compute-defaults)
(add-to-list 'grep-find-ignored-directories "logs")
(add-to-list 'grep-find-ignored-directories "cover")
(add-to-list 'grep-find-ignored-directories "vendor")
(add-to-list 'grep-find-ignored-files "TAGS")
(add-to-list 'grep-files-aliases (cons "php" "*.php *.phtml"))
(add-to-list 'grep-files-aliases (cons "js" "*.js"))

;; Projext
(require 'projext)
(projext-set-projectile-tags-command)
(add-hook 'kill-emacs-hook (lambda () (projext-close-current-project)))

;;Small fix for selection with shift+up
; More infos: http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00174.html
(if (tty-type)
    (progn
      (define-key input-decode-map "\e[1;2A" [S-up])
      (define-key input-decode-map "\e[1;2B" [S-down])
      (define-key input-decode-map "\e[1;2C" [S-right])
      (define-key input-decode-map "\e[1;2D" [S-left])))

;; Etags-select
(require 'etags-select)

;; Redo
(require 'redo)

;; ECB
(require 'ecb)
(ecb-layout-define "sliim-layout" left-right nil
  (if (fboundp (quote ecb-set-directories-buffer)) (ecb-set-directories-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (ecb-split-ver 0.75 t)
  (if (fboundp (quote ecb-set-methods-buffer)) (ecb-set-methods-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (if (fboundp (quote ecb-set-history-buffer)) (ecb-set-history-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1))))

(setq ecb-layout-name "left13"
      ecb-options-version "2.40"
      ecb-tip-of-the-day nil
      ecb-history-make-buckets (quote never)
      ecb-kill-buffer-clears-history (quote auto)
      ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2)
      ecb-show-sources-in-directories-buffer (quote ("left7" "left13" "left14" "left15" "sliim-layout"))
      ecb-layout-window-sizes (quote (("left15" (ecb-directories-buffer-name 0.22426470588235295 . 0.6419753086419753) (ecb-methods-buffer-name 0.22426470588235295 . 0.345679012345679)) ("left14" (ecb-directories-buffer-name 0.22058823529411764 . 0.7407407407407407) (ecb-history-buffer-name 0.22058823529411764 . 0.24691358024691357)) ("left13" (ecb-directories-buffer-name 0.22794117647058823 . 0.9876543209876543)) ("left7" (ecb-directories-buffer-name 0.22794117647058823 . 0.5555555555555556) (ecb-history-buffer-name 0.22794117647058823 . 0.18518518518518517) (ecb-methods-buffer-name 0.22794117647058823 . 0.24691358024691357)) ("sliim-layout" (ecb-directories-buffer-name 0.19047619047619047 . 0.9811320754716981) (ecb-methods-buffer-name 0.21164021164021163 . 0.6226415094339622)))))

;; Auto-complete
(require 'auto-complete)
(global-auto-complete-mode t)           ;enable global-mode
(make-local-variable 'ac-sources)
(setq ac-auto-start nil)                ;not automatically start
(setq ac-dwim 3)                        ;Do what i mean
(setq ac-override-local-map nil)        ;don't override local map
(setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq ac-sources '(ac-source-abbrev
                               ac-source-words-in-buffer
                               ac-source-symbols))))

;; PHP
(require 'php-mode)
(require 'php-extras)
(require 'flymake-php)
(require 'php-project)
(require 'php-test)
(when (file-exists-p "~/.emacs.d/php-manual")
  (setq php-manual-path "~/.emacs.d/php-manual"))

(add-hook 'php-mode-hook 'php-mode-personal-hook)

;; Python
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)
(require 'jedi)
(add-hook 'python-mode-hook 'python-mode-personal-hook)
(require 'nose)

;; Ruby
(setq rbenv-installation-dir "/usr/local/rbenv")
(require 'rbenv)
(global-rbenv-mode)

;; Emms
(require 'emms-setup)
(require 'emms-streams)
(require 'emms-info)
(require 'emms-info-mp3info)
(require 'emms-browser)
(emms-standard)
(emms-default-players)
(setq emms-source-file-default-directory "~/musics/")
(add-to-list 'emms-info-functions 'emms-info-mp3info)

;; Helm
(require 'helm-git)
(require 'helm-etags+)

;; Markdown
(add-hook 'markdown-mode-hook 'markdown-mode-personal-hook)
(setq markdown-xhtml-header-content "<meta charset='utf-8'>")

;; Ack
(setq ack-and-a-half-executable "ack-grep")

;; Eshell
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t))
(setq eshell-highlight-prompt nil)
(setq eshell-history-size 512)
(setq eshell-prompt-regexp "^[^#$]*[#$] ")
(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize ((lambda ()
                        (replace-regexp-in-string
                         (concat "/home/" (getenv "USER")) "~"
                         (eshell/pwd)))) 'face `(:foreground "yellow"))
         (or (git-current-branch (eshell/pwd)))
         (or (git-unpushed-commits (eshell/pwd)))
         (propertize " > " 'face 'default))))

;; Monky
(setq monky-process-type 'cmdserver)

;; DVC and helm support for mercurial
;; DVC Quick install:
;; - $ bzr get http://bzr.xsteve.at/dvc/ ~/.emacs.d/dvc-bzr
;; - $ cd ~/.emacs.d/dvc-bzr; autoconf; mkdir build; cd build; ../configure; make
;; - # make install
;; - $ mv ./lisp ~/.emacs.d/dvc; cd ~/.emacs.d/; rm -r dvc-bzr
;; More info on http://www.xsteve.at/prg/emacs_dvc/dvc.html and INSTALL file.
(when (file-exists-p "~/.emacs.d/dvc")
  (add-to-list 'load-path "~/.emacs.d/dvc")
  (when (require 'dvc-autoloads nil t)
    (require 'helm-ls-hg)))

;; Personal Keybindings
(require 'skbd)
(skbd-global-mode)
(define-key skbd-mode-map (kbd "M-x") 'helm-M-x)
(define-key skbd-mode-map (kbd "C-c h") 'helm-mini)
(define-key skbd-mode-map (kbd "M-<up>") 'windmove-up)
(define-key skbd-mode-map (kbd "M-<down>") 'windmove-down)
(define-key skbd-mode-map (kbd "M-<left>") 'windmove-left)
(define-key skbd-mode-map (kbd "M-<right>") 'windmove-right)
(define-key skbd-mode-map (kbd "C-?") 'ecb-toggle-ecb-windows)
(define-key skbd-mode-map (kbd "C-.") 'sr-speedbar-toggle)
(define-key skbd-mode-map (kbd "C-/") 'undo)
(define-key skbd-mode-map (kbd "C-§") 'redo)
(define-key skbd-mode-map (kbd "C-<tab>") 'auto-complete)

;; Load private config if exists
(let ((user-config-file "~/.emacsrc"))
  (when (file-exists-p user-config-file)
    (load-file user-config-file)))

;;; 020-config.el ends here
