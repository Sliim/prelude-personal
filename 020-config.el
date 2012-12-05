;;Global config
(setq-default tab-width 4)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq require-final-newline t)
(setq c-basic-offset 4)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)
(add-hook 'kill-emacs-hook (lambda () (projext-close-current-project)))

;; Add vendor/ dir to load-path
(add-to-list 'load-path "~/.emacs.d/prelude/personal/elisp")
(add-to-list 'load-path "~/.emacs.d/prelude/personal/vendor")

;;Prelude
(add-hook 'prog-mode-hook 'whitespace-turn-off t)
(add-hook 'text-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'turn-off-guru-mode t)
(add-hook 'prog-mode-hook
    (lambda nil (remove-hook 'before-save-hook 'whitespace-cleanup t))
    t)
(define-key prelude-mode-map (kbd "C-c C-g") 'prelude-google)

;; Projectile
(add-to-list 'projectile-globally-ignored-directories ".project")
(add-to-list 'projectile-globally-ignored-directories "logs")

;; Grep
(grep-compute-defaults)
(add-to-list 'grep-find-ignored-directories ".project")
(add-to-list 'grep-find-ignored-directories "logs")
(add-to-list 'grep-files-aliases (cons "php" "*.php *.phtml"))
(add-to-list 'grep-files-aliases (cons "js" "*.js"))

;; Projext
(require 'projext)
(projext-set-projectile-tags-command)
(define-key projectile-mode-map (kbd "C-c p t") 'projext-regenerate-tags)
(add-hook 'desktop-after-read-hook 'whitespace-turn-off t)

;;Small fix for selection with shift+up
; More infos: http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00174.html
(if (equal "xterm" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up]))

;; Etags-select
(require 'etags-select)
(define-key prelude-mode-map (kbd "C-x t") 'etags-select-find-tag)

;; Redo
(require 'redo)
(define-key prelude-mode-map (kbd "C-!") 'redo)

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

(setq ecb-tip-of-the-day nil)

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
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq ac-sources '(ac-source-abbrev
                               ac-source-files-in-current-dir
                               ac-source-words-in-buffer))))

(global-set-key (kbd "C-SPC") 'auto-complete)

;; PHP
(require 'php+-mode)
(require 'php-mode)
(require 'php-extras)
(require 'flymake-php)
(php+-mode-setup)

(when (file-exists-p "~/.emacs.d/php-manual")
  (setq php-manual-path "~/.emacs.d/php-manual"))

(add-hook 'php-mode-hook 'my-php-mode-hook)
(add-hook 'php-mode-hook 'flymake-php-load)
(add-hook 'php-mode-hook 'turn-on-eldoc-mode)
(add-hook 'php-mode-hook
          (lambda ()
            (c-set-offset 'case-label '+)))
(define-key php-mode-map (kbd "RET") 'newline-and-indent)

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
(define-key prelude-mode-map (kbd "C-c g") 'helm-git-find-files)
(define-key prelude-mode-map (kbd "C-c C-t") 'helm-etags+-select)

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
    (require 'helm-ls-hg)
    (define-key prelude-mode-map (kbd "C-c m") 'helm-hg-find-files-in-project)))

;;Personal Keybindings
(define-key prelude-mode-map (kbd "M-<up>") 'windmove-up)
(define-key prelude-mode-map (kbd "M-<down>") 'windmove-down)
(define-key prelude-mode-map (kbd "M-<left>") 'windmove-left)
(define-key prelude-mode-map (kbd "M-<right>") 'windmove-right)
(define-key prelude-mode-map (kbd "C-:") 'undo)
(define-key prelude-mode-map (kbd "C-,") 'ecb-show-ecb-windows)
(define-key prelude-mode-map (kbd "C-;") 'ecb-hide-ecb-windows)
