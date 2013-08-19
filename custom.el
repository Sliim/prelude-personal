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
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(ansi-term-color-vector ["#3f3f3f" "#cc9393" "#7f9f7f" "#f0dfaf" "#8cd0d3" "#dc8cc3" "#93e0e3" "#dcdccc"] t)
 '(custom-safe-themes (quote ("7fa9dc3948765d7cf3d7a289e40039c2c64abf0fad5c616453b263b601532493" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "5195dfc4aa4e8ff66248b9ba08983da04aff1d82e680fb9e008091fdb39d7c76" "9f42bccce1e13fa5017eb8718574db099e85358b9f424db78e7318f86d1be08f" "0bac11bd6a3866c6dee5204f76908ec3bdef1e52f3c247d5ceca82860cccfa9d" "27b53b2085c977a8919f25a3a76e013ef443362d887d52eaa7121e6f92434972" default)))
 '(fci-rule-color "#383838")
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#d54e53") (40 . "#e78c45") (60 . "#e7c547") (80 . "#b9ca4a") (100 . "#70c0b1") (120 . "#7aa6da") (140 . "#c397d8") (160 . "#d54e53") (180 . "#e78c45") (200 . "#e7c547") (220 . "#b9ca4a") (240 . "#70c0b1") (260 . "#7aa6da") (280 . "#c397d8") (300 . "#d54e53") (320 . "#e78c45") (340 . "#e7c547") (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))

;; (custom-theme-set-faces
;;  'ir-black
;;  '(default ((t (:background "black" :foreground "white" :height 85 :width normal))))
;;  '(region ((t (:background "SkyBlue4"))))
;;  '(font-lock-comment-face ((t (:foreground "grey30"))))
;;  '(font-lock-comment-delimiter-face ((t (:foreground "grey30"))))
;;  '(font-lock-constant-face ((t (:foreground "grey50"))))
;;  '(hl-line-face ((t (:background "grey15"))))
;;  '(hl-line ((t (:background "grey15"))))
;;  '(ac-candidate-face ((t (:background "gray50" :foreground "black"))))
;;  '(ac-candidate-mouse-face ((t (:background "gray50" :foreground "black"))))
;;  '(ac-etags-candidate-face ((t (:background "grey50" :foreground "black"))) t)
;;  '(ac-etags-selection-face ((t (:background "grey15" :foreground "SteelBlue"))) t)
;;  '(ac-selection-face ((t (:background "grey15" :foreground "SteelBlue"))))
;;  '(ecb-default-highlight-face ((((class color) (background dark)) (:background "grey15"))))
;;  '(ecb-default-highlight-face ((t (:background "DodgerBlue2"))))
;;  '(mode-line ((t (:foreground "SkyBlue2" :background "grey30"))))
;;  '(mode-line-buffer-id ((t (:foreground "DeepSkyBlue"))))
;;  '(mode-line-inactive ((t (:foreground "DodgerBlue3" :background "grey30"))))
;;  '(minibuffer-prompt ((t (:foreground "LightSkyBlue"))))
;;  '(helm-header ((t (:foreground "SteelBlue"))))
;;  '(helm-source-header ((t (:foreground "LightSkyBlue"))))
;;  '(helm-candidate-number ((t (:foreground "LightSkyBlue"))))
;;  '(hl-line ((t (:underline nil))))
;;  '(cursor ((t (:background "LightSkyBlue"))))
;;  '(vertical-border ((t (:foreground "LightSkyBlue4")))))

(custom-theme-set-faces
 'sanityinc-tomorrow-bright
 '(default ((t (:background "black" :foreground "white" :height 85 :width normal))))
 '(font-lock-comment-face ((t (:foreground "grey30"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "grey30"))))
 '(font-lock-constant-face ((t (:foreground "grey50"))))
 '(ac-candidate-face ((t (:background "gray50" :foreground "black"))))
 '(ac-candidate-mouse-face ((t (:background "gray50" :foreground "black"))))
 '(ac-etags-candidate-face ((t (:background "grey50" :foreground "black"))) t)
 '(ac-etags-selection-face ((t (:background "grey15" :foreground "SteelBlue"))) t)
 '(ac-selection-face ((t (:background "grey15" :foreground "SteelBlue"))))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "grey15"))))
 '(ecb-default-highlight-face ((t (:background "DodgerBlue2"))))
 '(helm-header ((t (:foreground "SteelBlue"))))
 '(helm-source-header ((t (:foreground "LightSkyBlue"))))
 '(helm-candidate-number ((t (:foreground "LightSkyBlue"))))
 '(hl-line ((t (:underline nil))))
 '(vertical-border ((t (:foreground "LightSkyBlue4")))))

;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-grep-match ((t (:background "DeepSkyBlue4" :foreground "black"))))
 '(helm-match ((t (:background "DeepSkyBlue4" :foreground "black"))))
 '(helm-selection ((t (:background "#628c98" :foreground "black" :underline nil)))))
