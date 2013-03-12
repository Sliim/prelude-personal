;;; custom.el --- Emacs Prelude: Customizations file.
;;
;; Author: Sliim <sliim@mailoo.org>
;; Version: 1.0.0
;; Keywords: prelude personal

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Customizations file

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(ansi-term-color-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"] t)
 '(custom-safe-themes (quote ("0bac11bd6a3866c6dee5204f76908ec3bdef1e52f3c247d5ceca82860cccfa9d" "27b53b2085c977a8919f25a3a76e013ef443362d887d52eaa7121e6f92434972" default)))
 '(delete-trailing-lines t)
 '(ecb-history-make-buckets (quote never))
 '(ecb-kill-buffer-clears-history (quote auto))
 '(ecb-layout-name "left13")
 '(ecb-layout-window-sizes (quote (("left15" (ecb-directories-buffer-name 0.22426470588235295 . 0.6419753086419753) (ecb-methods-buffer-name 0.22426470588235295 . 0.345679012345679)) ("left14" (ecb-directories-buffer-name 0.22058823529411764 . 0.7407407407407407) (ecb-history-buffer-name 0.22058823529411764 . 0.24691358024691357)) ("left13" (ecb-directories-buffer-name 0.22794117647058823 . 0.9876543209876543)) ("left7" (ecb-directories-buffer-name 0.22794117647058823 . 0.5555555555555556) (ecb-history-buffer-name 0.22794117647058823 . 0.18518518518518517) (ecb-methods-buffer-name 0.22794117647058823 . 0.24691358024691357)) ("sliim-layout" (ecb-directories-buffer-name 0.19047619047619047 . 0.9811320754716981) (ecb-methods-buffer-name 0.21164021164021163 . 0.6226415094339622)))))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-show-sources-in-directories-buffer (quote ("left7" "left13" "left14" "left15" "sliim-layout")))
 '(fci-rule-color "#383838")
 '(global-auto-revert-mode t)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount (quote (2 ((shift) . 2) ((control)))))
 '(phpcs-shell-command "/usr/bin/phpcs")
 '(phpcs-standard "PSR2")
 '(phpunit-shell-command "/usr/bin/phpunit")
 '(prelude-flyspell nil)
 '(prelude-guru nil)
 '(prelude-whitespace nil)
 '(ps-font-size 8)
 '(ps-header-font-size 9)
 '(ps-header-title-font-size 10)
 '(ps-line-number t)
 '(ps-line-number-font-size 8)
 '(ps-line-number-step 1)
 '(ps-print-color-p (quote black-white))
 '(scroll-bar-mode nil)
 '(scroll-conservatively 10000)
 '(scroll-step 1)
 '(markdown-xhtml-header-content "<meta charset='utf-8'>"))

(custom-theme-set-faces
 'zenburn
 '(default ((t (:background "grey10" :foreground "white" :height 85 :width normal))))
 '(region ((t (:background "SkyBlue4"))))
 '(font-lock-comment-face ((t (:foreground "grey30"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "grey30"))))
 '(font-lock-constant-face ((t (:foreground "grey50"))))
 '(hl-line-face ((t (:background "grey15"))))
 '(hl-line ((t (:background "grey15"))))
 '(ac-candidate-face ((t (:background "gray50" :foreground "black"))))
 '(ac-candidate-mouse-face ((t (:background "gray50" :foreground "black"))))
 '(ac-etags-candidate-face ((t (:background "grey50" :foreground "black"))) t)
 '(ac-etags-selection-face ((t (:background "grey15" :foreground "SteelBlue"))) t)
 '(ac-selection-face ((t (:background "grey15" :foreground "SteelBlue"))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "grey15"))))
 '(ecb-default-highlight-face ((t (:background "DodgerBlue2"))))
 '(mode-line ((t (:foreground "SkyBlue2" :background "grey30"))))
 '(mode-line-buffer-id ((t (:foreground "DeepSkyBlue"))))
 '(mode-line-inactive ((t (:foreground "DodgerBlue3" :background "grey30"))))
 '(minibuffer-prompt ((t (:foreground "LightSkyBlue"))))
 '(helm-header ((t (:foreground "SteelBlue"))))
 '(helm-source-header ((t (:foreground "LightSkyBlue"))))
 '(helm-candidate-number ((t (:foreground "LightSkyBlue")))))

(custom-theme-set-faces
 'ir-black
 '(hl-line ((t (:underline nil))))
 '(cursor ((t (:background "LightSkyBlue")))))

;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-grep-match ((t (:background "DeepSkyBlue4" :foreground "black"))))
 '(helm-match ((t (:background "DeepSkyBlue4" :foreground "black"))))
 '(helm-selection ((t (:background "#628c98" :foreground "black" :underline nil)))))
