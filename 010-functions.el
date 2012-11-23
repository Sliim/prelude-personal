;;;;;;;; Project Management functions ;;;;;;;;
(defvar current-project nil)

(defun project-open ()
  "Function that open project, load snippets, visit tags table and read project desktop if exists"
  (interactive)
  (let ((project (php-project-ask-for-project "Project: ")))
    (project-close-current)
    (php-project-dired-directory project)
    (setq project-emacs-dir (concat (php-project-directory project) ".emacs/"))
    (when (and (/= (length (php-project-tags-file project)) 0)
               (file-exists-p (php-project-tags-file project)))
      (message "Loading project's tags table..")
      (tags-reset-tags-tables)
      (visit-tags-table (php-project-tags-file project)))
    (when (file-exists-p project-emacs-dir)
      (setq desktop-path '(concat (php-project-directory project) ".emacs/"))
      (setq desktop-dirname project-emacs-dir)
      (setq project-snippets-dir (concat project-emacs-dir "snippets/"))

      (when (file-exists-p project-snippets-dir)
        (message "Loading project's snippets..")
        (add-to-list 'yas/root-directory project-snippets-dir)
        (mapc 'yas/load-directory yas/root-directory))
      (when (file-exists-p (concat project-emacs-dir ".emacs.desktop"))
        (message "Loading project's desktop..")
        (desktop-read))
      (when (file-exists-p (concat project-emacs-dir "project-config.el"))
        (message "Loading project's configuration..")
        (load-file (concat project-emacs-dir "project-config.el"))))
    (setq current-project project)
    (setq tags-completion-table nil)
    (set-projectile-tags-command)
    (message (concat "Project " (php-project-nickname current-project) " opened."))))

(defun project-save-desktop ()
  "Function that save current desktop in .emacs's project dir"
  (interactive)
  (if current-project
      (when (file-exists-p (concat (php-project-directory current-project) "/.emacs/"))
        (desktop-save (concat (php-project-directory current-project) ".emacs/"))
        (message "Desktop saved."))
    (message "No project opened..")))

(defun project-show-current ()
  "Show the current project"
  (interactive)
  (if current-project
      (message (php-project-nickname current-project))
    (message "none")))

(defun project-close-current ()
  "Close current project"
  (interactive)
  (when current-project
    (when (y-or-n-p (concat "Save desktop for current project " (php-project-nickname current-project) " "))
      (project-save-desktop))
    (desktop-clear)
    (project-desktop-remove-lock-file)
    (message (concat "Project " (php-project-nickname current-project) " closed."))
    (setq current-project nil)
    (set-projectile-tags-command)))

(defun project-clear-desktop ()
  "Overload desktop-clear to open current project directory when clearing desktop"
  (interactive)
  (desktop-clear)
  (when current-project
    (php-project-dired-directory current-project)))

(defun project-desktop-remove-lock-file ()
  "Remove desktop lock file"
  (when current-project
    (setq project-desktop-lock-file (concat (php-project-directory current-project) ".emacs/.emacs.desktop.lock"))
    (when (file-exists-p project-desktop-lock-file)
      (shell-command (concat "rm " project-desktop-lock-file))
      (message "Desktop lock file removed."))))
;;;;;;;; End of Project Management functions ;;;;;;;;

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

(defun set-projectile-tags-command ()
  "Set projectile-tags-command custom variable"
  (setq projectile-tags-command "ctags-exuberant -Re \
    --languages=PHP \
    --exclude=\"\.git\" \
    --totals=yes \
    --tag-relative=yes \
    --PHP-kinds=-v \
    --regex-PHP='/abstract class ([^ ]*)/\1/c/' \
    --regex-PHP='/trait ([^ ]*)/\1/c/' \
    --regex-PHP='/interface ([^ ]*)/\1/c/' \
    --regex-PHP='/(public |final |static |abstract |protected |private )+function ([^ (]*)/\2/f/' \
    --regex-PHP='/const ([^ ]*)/\1/d/'")

  (when current-project
    (when (/= (length (php-project-tags-file current-project)) 0)
      (setq projectile-tags-command (concat projectile-tags-command " -o " (php-project-tags-file current-project))))))
