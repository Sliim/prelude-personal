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

;;Load theme
(load-theme 'sanityinc-tomorrow-bright t)

;;Global config
(scroll-bar-mode -1)
(setq-default tab-width 4)
(setq confirm-kill-emacs 'yes-or-no-p
      require-final-newline t
      c-basic-offset 4
      scroll-conservatively 10000
      scroll-step 1
      delete-trailing-lines t
      global-auto-revert-mode t
      mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount (quote (2 ((shift) . 2) ((control))))
      vc-follow-symlinks t)

;; Print config
(setq ps-font-size 8
      ps-header-font-size 9
      ps-header-title-font-size 10
      ps-line-number t
      ps-line-number-font-size 8
      ps-line-number-step 1
      ps-print-color-p (quote black-white))

;; Changelog config
(setq add-log-full-name "Sliim"
      add-log-mailing-address "sliim@mailoo.org")

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

;; Projext
(require 'projext)
(projext-init)

;; Projectile
(add-to-list 'projectile-globally-ignored-directories ".project")
(setq projectile-globally-ignored-files '())

;; Project-persist
(require 'project-persist)
(require 'helm-project-persist)
(setq project-persist-settings-dir (concat user-emacs-directory "var/project-persist"))
(setq project-persist-auto-save-global nil)

;; Grep
(grep-compute-defaults)
(add-to-list 'grep-find-ignored-directories "logs")
(add-to-list 'grep-find-ignored-directories "cover")
(add-to-list 'grep-find-ignored-directories "vendor")
(add-to-list 'grep-find-ignored-files "TAGS")
(add-to-list 'grep-files-aliases (cons "php" "*.php *.phtml"))
(add-to-list 'grep-files-aliases (cons "js" "*.js"))

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
(when (file-exists-p "~/.emacs.d/php-manual")
  (setq php-manual-path "~/.emacs.d/php-manual"))

(add-hook 'php-mode-hook 'php-mode-personal-hook)

;; Python
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)
(require 'jedi)
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
(elpy-clean-modeline)
(setq elpy-rpc-bkend "rope")
(add-hook 'python-mode-hook 'python-mode-personal-hook)

;; Ruby
(if (file-exists-p "~/.rbenv")
    (progn
      (setq rbenv-installation-dir "~/.rbenv")
      (require 'rbenv)
      (global-rbenv-mode)))


;; Javascript
;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js-mode-hook 'flymake-jslint-load)
(require 'js-comint)
(setq inferior-js-program-command "node")
(setq inferior-js-mode-hook
      (lambda ()
        (ansi-color-for-comint-mode-on)
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[JGK]" "" output)))))

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
(setq helm-buffers-favorite-modes (quote (emacs-lisp-mode org-mode php-mode ruby-mode python-mode shell-script-mode))
      helm-follow-mode-persistent t)
(setq helm-etags+-use-short-file-name 'absolute)

;; Markdown
(add-hook 'markdown-mode-hook 'markdown-mode-personal-hook)
(setq markdown-xhtml-header-content "<meta charset='utf-8'>")

;; Git-messenger
(require 'git-messenger)
(setq git-messenger:show-detail t)

;; Eshell
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t))
(setq eshell-highlight-prompt nil)
(setq eshell-history-size 512)
(setq eshell-directory-name (concat (getenv "HOME") "/.eshell"))
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
(when (file-exists-p "~/.emacs.d/dvc")
  (add-to-list 'load-path "~/.emacs.d/dvc")
  (when (require 'dvc-autoloads nil t)
    (require 'helm-ls-hg)))

;; Popwin
(require 'popwin)
(popwin-mode 1)
(push '("*quickrun*" :height 20) popwin:special-display-config)

;; Direx
(require 'direx)
(push '(direx:direx-mode :position left :width 25 :dedicated t) popwin:special-display-config)

;; Powerline
(require 'powerline)
(powerline-default-theme)
(setq powerline-default-separator 'arrow-fade)

;; Perspective
(require 'perspective)
(persp-mode 1)

;; Personal Keybindings
(require 'skbd)
(skbd-global-mode)
(define-key skbd-mode-map (kbd "M-x") 'helm-M-x)
(define-key skbd-mode-map (kbd "C-c h") 'helm-mini)
(define-key skbd-mode-map (kbd "M-<up>") 'windmove-up)
(define-key skbd-mode-map (kbd "M-<down>") 'windmove-down)
(define-key skbd-mode-map (kbd "M-<left>") 'windmove-left)
(define-key skbd-mode-map (kbd "M-<right>") 'windmove-right)
(define-key skbd-mode-map (kbd "C-<tab>") 'auto-complete)

(key-chord-define-global "ca" 'auto-complete)
(key-chord-define-global "cj" 'jedi:complete)

;; Load private config if exists
(let ((user-config-file "~/.emacsrc"))
  (when (file-exists-p user-config-file)
    (load-file user-config-file)))

;;; 020-config.el ends here
