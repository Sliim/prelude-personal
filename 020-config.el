;;Prelude config
(add-hook 'prog-mode-hook 'whitespace-turn-off t)
(add-hook 'text-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'turn-off-flyspell t)
(add-hook 'prog-mode-hook 'turn-off-guru-mode t)
(add-hook 'prog-mode-hook
    (lambda nil (remove-hook 'before-save-hook 'whitespace-cleanup t))
    t)

;;Global config
(setq-default tab-width 4)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq require-final-newline t)
(setq c-basic-offset 4)
(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;;Small fix for selection with shift+up
; More infos: http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00174.html
(if (equal "xterm" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up]))

;; Vendor path
;; Here is copy of el packages that are not indexed by elpa repositories
(when (file-exists-p "~/.emacs.d/vendor")
  (add-to-list 'load-path "~/.emacs.d/vendor")

  ;; Etags-select
  (require 'etags-select)
  (global-set-key (kbd "C-c C-t") 'etags-select-find-tag)

    ;; Redo
  (require 'redo)
  (global-set-key (kbd "C-!") 'redo))

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
(ecb-layout-define "sliim-left-layout" left nil
  (ecb-split-ver 0.8125 t)
  (if (fboundp (quote ecb-set-history-buffer)) (ecb-set-history-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (if (fboundp (quote ecb-set-directories-buffer)) (ecb-set-directories-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 1) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (if (fboundp (quote ecb-set-history-buffer)) (ecb-set-history-buffer) (ecb-set-default-ecb-buffer))
  (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1)))
  (dotimes (i 2) (other-window 1) (if (equal (selected-window) ecb-compile-window) (other-window 1))))

(setq ecb-tip-of-the-day nil)

;; Auto-complete
(when (require 'auto-complete nil t)
   (global-auto-complete-mode t)           ;enable global-mode
   (make-local-variable 'ac-sources)
   (setq ac-auto-start nil)                ;not automatically start
   (setq ac-dwim 3)                        ;Do what i mean
   (setq ac-override-local-map nil)        ;don't override local map
   (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer))
   (add-hook 'emacs-lisp-mode-hook
             (lambda ()
               (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-buffer
                                                      ac-source-symbols))))
   (add-hook 'eshell-mode-hook
             (lambda ()
               (setq ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-files-in-current-dir
                                                      ac-source-words-in-buffer))))
   (global-set-key (kbd "C-SPC") 'auto-complete))

;; PHP
(require 'php+-mode)
(require 'php-mode)
(require 'php-extras)
(require 'flymake-php)
(php+-mode-setup)

(when (file-exists-p "~/.emacs.d/php-manual")
  (setq php-manual-path "~/.emacs.d/php-manual"))

(defun my-php-mode-hook ()
  (interactive)
  "Function to be called when entering into php-mode."
  (when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))
    (make-local-variable 'ac-sources)
    (setq ac-sources '(ac-source-words-in-same-mode-buffers
                       ac-source-dictionary))
    (when (require 'auto-complete-etags nil t)
      (add-to-list 'ac-sources 'ac-source-etags))
    (auto-complete-mode t)))
(add-hook 'php-mode-hook 'my-php-mode-hook)
(add-hook 'php-mode-hook 'flymake-php-load)
(add-hook 'php-mode-hook 'turn-on-eldoc-mode)
(add-hook 'php-mode-hook
          (lambda ()
            (c-set-offset 'case-label '+)))
(define-key php-mode-map (kbd "RET") 'newline-and-indent)

;; Emms
(require 'emms-setup)
(emms-standard)
(emms-default-players)
(setq emms-source-file-default-directory "~/musics/")

;; Geben
(require 'geben)

;; Multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; Grep
(grep-compute-defaults)
(add-to-list 'grep-find-ignored-directories ".emacs")
(add-to-list 'grep-find-ignored-directories "logs")
(add-to-list 'grep-files-aliases (cons "php" "*.php *.phtml"))
(add-to-list 'grep-files-aliases (cons "js" "*.js"))
(setq grep-find-template "find -L . <X> -type f <F> -print0 | \"xargs\" -0 -e grep <C> -nH -e <R>")
(defun grep-compute-defaults ()
"Hack to keep my changes...")

;;Personal Keybindings
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "C-:") 'undo)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-,") 'ecb-show-ecb-windows)
(global-set-key (kbd "C-;") 'ecb-hide-ecb-windows)
