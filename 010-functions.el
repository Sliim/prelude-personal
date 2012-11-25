(defun untabify-buffer ()
  "Untabify the entire buffer."
  (interactive)
  (untabify (point-min) (point-max))
)

(defun ecb-refresh ()
  "Refresh ecb window (directories, methods, history)"
  (interactive)
  (ecb-update-directories-buffer)
  (ecb-clear-history)
  (ecb-rebuild-methods-buffer))
