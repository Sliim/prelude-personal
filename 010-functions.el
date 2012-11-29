(defun ecb-refresh ()
  "Refresh ecb window (directories, methods, history)"
  (interactive)
  (ecb-update-directories-buffer)
  (ecb-clear-history)
  (ecb-rebuild-methods-buffer))

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
