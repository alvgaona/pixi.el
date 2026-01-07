;;; pixi.el --- Pixi integration for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2025 Alvaro

;; Author: Alvaro
;; Maintainer: Alvaro
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, processes, python
;; URL: https://github.com/alvgaona/pixi.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; pixi.el provides Emacs integration for Pixi, the fast cross-platform
;; package manager built on the Conda ecosystem.
;;
;; Features:
;; - Auto-detect and activate pixi projects
;; - Environment switching with completion
;; - Modeline indicator showing active environment
;; - Task running with completion (pixi-run-task, pixi-run)
;; - Package management (pixi-add, pixi-remove, pixi-install, etc.)
;; - Workspace commands (channels, platforms, features)
;; - Eglot/LSP auto-restart on environment change
;; - Hooks for customization
;;
;; Basic usage:
;;   (require 'pixi)
;;   (setq pixi-auto-activate t)
;;
;; Or with use-package:
;;   (use-package pixi
;;     :config
;;     (setq pixi-auto-activate t))

;;; Code:

(require 'project)

;;; Customization

(defgroup pixi nil
  "Pixi integration for Emacs."
  :group 'tools
  :prefix "pixi-")

(defcustom pixi-executable
  (or (executable-find "pixi")
      (expand-file-name "~/.pixi/bin/pixi"))
  "Path to the pixi executable."
  :type 'string
  :group 'pixi)

(defcustom pixi-auto-activate t
  "Automatically activate pixi when opening a file in a pixi project."
  :type 'boolean
  :group 'pixi)

(defcustom pixi-show-modeline t
  "Whether to show pixi status in the modeline."
  :type 'boolean
  :group 'pixi)

(defcustom pixi-modeline-show-project t
  "Whether to show project name in the modeline."
  :type 'boolean
  :group 'pixi)

(defcustom pixi-modeline-show-environment t
  "Whether to show environment name in the modeline."
  :type 'boolean
  :group 'pixi)

(defcustom pixi-modeline-prefix "pixi"
  "Prefix string to show in the modeline. Set to nil to hide."
  :type '(choice (string :tag "Prefix")
                 (const :tag "None" nil))
  :group 'pixi)

;;; Internal variables

(defvar pixi--original-process-environment nil
  "Saved process environment before pixi activation.")

(defvar pixi--current-project nil
  "Path to the currently active pixi project root.")

(defvar pixi--current-environment nil
  "Name of the currently active pixi environment.")

;;; Hooks

(defvar pixi-activate-hook nil
  "Hook run after activating a pixi environment.")

(defvar pixi-deactivate-hook nil
  "Hook run after deactivating a pixi environment.")

(defvar pixi-pre-run-hook nil
  "Hook run before running a pixi task or command.")

;;; Modeline

(defun pixi--modeline-string ()
  "Return the modeline string for pixi status."
  (when (and pixi-show-modeline pixi--current-project)
    (let ((parts '()))
      (when pixi-modeline-prefix
        (setq parts (append parts (list pixi-modeline-prefix))))
      (when pixi-modeline-show-project
        (setq parts (append parts (list (file-name-nondirectory (directory-file-name pixi--current-project))))))
      (when pixi-modeline-show-environment
        (setq parts (append parts (list pixi--current-environment))))
      (when parts
        (format " %s" (string-join parts ":"))))))

(defvar pixi-mode-line
  '(:eval (pixi--modeline-string))
  "Modeline indicator for pixi status.")

(put 'pixi-mode-line 'risky-local-variable t)

(defun pixi-modeline-enable ()
  "Enable pixi modeline indicator."
  (interactive)
  (setq pixi-show-modeline t)
  (add-to-list 'mode-line-misc-info 'pixi-mode-line t))

(defun pixi-modeline-disable ()
  "Disable pixi modeline indicator."
  (interactive)
  (setq pixi-show-modeline nil)
  (setq mode-line-misc-info (delete 'pixi-mode-line mode-line-misc-info)))

(when pixi-show-modeline
  (add-to-list 'mode-line-misc-info 'pixi-mode-line t))

;;; Detection functions

(defun pixi--find-project-root (&optional dir)
  "Find the root of a pixi project starting from DIR.
Looks for pixi.toml or pyproject.toml containing [tool.pixi].
Returns the directory path or nil if not found."
  (let ((start-dir (or dir default-directory)))
    (or
     ;; First, look for pixi.toml (native pixi project)
     (locate-dominating-file start-dir "pixi.toml")
     ;; Then, look for pyproject.toml with [tool.pixi]
     (locate-dominating-file start-dir
                             (lambda (dir)
                               (pixi--pyproject-has-pixi-p dir))))))

(defun pixi--pyproject-has-pixi-p (dir)
  "Check if DIR contains a pyproject.toml with [tool.pixi] section."
  (let ((pyproject (expand-file-name "pyproject.toml" dir)))
    (when (file-exists-p pyproject)
      (with-temp-buffer
        (insert-file-contents pyproject)
        (goto-char (point-min))
        (re-search-forward "^\\[tool\\.pixi\\]" nil t)))))

(defun pixi--project-type (&optional dir)
  "Determine the pixi project type at DIR.
Returns \\='pixi-toml, \\='pyproject, or nil."
  (let ((root (pixi--find-project-root dir)))
    (when root
      (cond
       ((file-exists-p (expand-file-name "pixi.toml" root)) 'pixi-toml)
       ((file-exists-p (expand-file-name "pyproject.toml" root)) 'pyproject)))))

(defun pixi--config-file (&optional dir)
  "Return the path to the pixi config file for project at DIR."
  (let ((root (pixi--find-project-root dir)))
    (when root
      (pcase (pixi--project-type root)
        ('pixi-toml (expand-file-name "pixi.toml" root))
        ('pyproject (expand-file-name "pyproject.toml" root))))))

;;; Task parsing

(defun pixi--list-tasks (&optional project-root)
  "List available tasks for PROJECT-ROOT.
Returns a list of task name strings."
  (let* ((root (or project-root (pixi--find-project-root)))
         (default-directory (or root default-directory))
         (output (shell-command-to-string
                  (format "%s task list --json 2>/dev/null" pixi-executable))))
    (when (and output (not (string-empty-p output)))
      (let* ((json (json-read-from-string output))
             (tasks (make-hash-table :test 'equal)))
        ;; Collect unique task names from all environments
        (dolist (env (append json nil))
          (dolist (feature (append (alist-get 'features env) nil))
            (dolist (task (append (alist-get 'tasks feature) nil))
              (puthash (alist-get 'name task) t tasks))))
        (hash-table-keys tasks)))))

;;; Environment activation

(defun pixi--get-environment-from-shell-hook (project-root &optional env-name)
  "Get environment variables by running pixi shell-hook at PROJECT-ROOT.
Optional ENV-NAME specifies which environment (default, dev, etc.)."
  (let* ((default-directory project-root)
         (env-arg (if env-name (format " -e %s" env-name) ""))
         (cmd (format "eval \"$(%s shell-hook --shell bash%s)\" && env"
                      pixi-executable env-arg))
         (output (shell-command-to-string (format "bash -c %s" (shell-quote-argument cmd)))))
    (when (not (string-empty-p output))
      (split-string output "\n" t))))

(defun pixi--parse-env-line (line)
  "Parse an environment LINE into (NAME . VALUE) cons cell."
  (when (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
    (cons (match-string 1 line) (match-string 2 line))))

(defun pixi-activate (&optional project-root env-name)
  "Activate the pixi environment for PROJECT-ROOT.
If PROJECT-ROOT is nil, detect from current directory.
ENV-NAME specifies the environment (\"default\" if nil)."
  (interactive)
  (let ((root (or project-root (pixi--find-project-root)))
        (env (or env-name "default")))
    (unless root
      (user-error "Not in a pixi project"))
    ;; Save original environment if not already saved
    (unless pixi--original-process-environment
      (setq pixi--original-process-environment (copy-sequence process-environment)))
    ;; Get new environment
    (let ((new-env (pixi--get-environment-from-shell-hook root env)))
      (unless new-env
        (user-error "Failed to get pixi environment"))
      ;; Apply new environment
      (setq process-environment new-env)
      ;; Update exec-path from new PATH
      (when-let ((path (getenv "PATH")))
        (setq exec-path (parse-colon-path path)))
      ;; Track current project
      (setq pixi--current-project root)
      (setq pixi--current-environment env)
      (run-hooks 'pixi-activate-hook)
      (message "Activated pixi: %s [%s]"
               (file-name-nondirectory (directory-file-name root))
               env))))

(defun pixi-deactivate ()
  "Deactivate the current pixi environment, restoring the original."
  (interactive)
  (unless pixi--original-process-environment
    (user-error "No pixi environment is active"))
  (setq process-environment pixi--original-process-environment)
  ;; Restore exec-path
  (when-let ((path (getenv "PATH")))
    (setq exec-path (parse-colon-path path)))
  ;; Clear tracking
  (setq pixi--original-process-environment nil)
  (setq pixi--current-project nil)
  (setq pixi--current-environment nil)
  (run-hooks 'pixi-deactivate-hook)
  (message "Pixi environment deactivated"))

(defun pixi-active-p ()
  "Return non-nil if a pixi environment is currently active."
  (and pixi--current-project t))

(defun pixi--list-environments (&optional project-root)
  "List available environments for PROJECT-ROOT."
  (let* ((root (or project-root (pixi--find-project-root)))
         (default-directory root)
         (output (shell-command-to-string
                  (format "%s workspace environment list 2>/dev/null" pixi-executable))))
    (when (not (string-empty-p output))
      (let (envs)
        (dolist (line (split-string output "\n" t))
          (when (string-match "^- \\([^:]+\\):" line)
            (push (match-string 1 line) envs)))
        (nreverse envs)))))

(defun pixi-switch-environment (env-name)
  "Switch to a different pixi environment ENV-NAME within the current project."
  (interactive
   (list
    (let ((envs (pixi--list-environments)))
      (unless envs
        (user-error "No environments found (not in a pixi project?)"))
      (completing-read "Environment: " envs nil t))))
  (let ((root (or pixi--current-project (pixi--find-project-root))))
    (unless root
      (user-error "Not in a pixi project"))
    ;; If already active, we need to get fresh env from original
    (when pixi--original-process-environment
      (setq process-environment (copy-sequence pixi--original-process-environment)))
    ;; Activate with new environment
    (pixi-activate root env-name)))

;;; Display buffer

(defun pixi--display-buffer (title content)
  "Display CONTENT with TITLE in the *pixi* buffer (read-only)."
  (with-current-buffer (get-buffer-create "*pixi*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert title)
      (insert "\n\n")
      (if (listp content)
          (dolist (item content)
            (insert (format "  %s\n" item)))
        (insert content)))
    (goto-char (point-min))
    (special-mode)
    (display-buffer (current-buffer))))

;;; Interactive commands

(defun pixi-info ()
  "Display information about the current pixi project."
  (interactive)
  (let ((root (pixi--find-project-root)))
    (if root
        (pixi--display-buffer
         "Pixi Project Information"
         (format "Project: %s\nType: %s"
                 root
                 (pixi--project-type root)))
      (message "Not in a pixi project"))))

(defun pixi-list-environments ()
  "List available environments for the current pixi project."
  (interactive)
  (let ((envs (pixi--list-environments)))
    (if envs
        (pixi--display-buffer
         "Pixi Environments"
         (append
          envs
          (when pixi--current-environment
            (list (format "\nActive: %s" pixi--current-environment)))))
      (message "No environments found (not in a pixi project?)"))))

(defun pixi-list-tasks ()
  "List available tasks for the current pixi project."
  (interactive)
  (if (pixi--find-project-root)
      (let ((tasks (pixi--list-tasks)))
        (if tasks
            (pixi--display-buffer "Pixi Tasks" tasks)
          (message "No tasks defined")))
    (message "Not in a pixi project")))

(defun pixi-run-task (task &optional args)
  "Run a pixi TASK with optional ARGS using compile-mode."
  (interactive
   (let* ((tasks (pixi--list-tasks))
          (task (completing-read "Task: " tasks nil t))
          (args (read-string "Args (optional): ")))
     (list task (unless (string-empty-p args) args))))
  (run-hooks 'pixi-pre-run-hook)
  (let* ((root (pixi--find-project-root))
         (default-directory root)
         (cmd (if args
                  (format "%s run %s %s" pixi-executable task args)
                (format "%s run %s" pixi-executable task))))
    (compile cmd)))

(defun pixi-run (command)
  "Run an arbitrary COMMAND with pixi run."
  (interactive "spixi run: ")
  (run-hooks 'pixi-pre-run-hook)
  (let* ((root (pixi--find-project-root))
         (default-directory (or root default-directory))
         (cmd (format "%s run %s" pixi-executable command)))
    (compile cmd)))

;;; Package management

(defun pixi--setup-output-buffer ()
  "Set up the *pixi* buffer with special-mode after command completes."
  (when-let ((buffer (get-buffer "*pixi*"))
             (process (get-buffer-process buffer)))
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (with-current-buffer (process-buffer proc)
           (special-mode)))))))

(defun pixi--run-command (args)
  "Run pixi with ARGS and display output in *pixi* buffer."
  (let* ((root (pixi--find-project-root))
         (default-directory (or root default-directory))
         (cmd (format "%s %s" pixi-executable args)))
    (async-shell-command cmd "*pixi*")
    (pixi--setup-output-buffer)))

(defun pixi-add (package &optional feature)
  "Add PACKAGE as a dependency, optionally to FEATURE."
  (interactive
   (let* ((pkg (read-string "Package to add: "))
          (feat (read-string "Feature (optional): ")))
     (list pkg (unless (string-empty-p feat) feat))))
  (pixi--run-command
   (if feature
       (format "add --feature %s %s" feature package)
     (format "add %s" package))))

(defun pixi-add-pypi (package &optional feature)
  "Add PACKAGE from PyPI, optionally to FEATURE."
  (interactive
   (let* ((pkg (read-string "PyPI package to add: "))
          (feat (read-string "Feature (optional): ")))
     (list pkg (unless (string-empty-p feat) feat))))
  (pixi--run-command
   (if feature
       (format "add --pypi --feature %s %s" feature package)
     (format "add --pypi %s" package))))

(defun pixi-remove (package)
  "Remove PACKAGE from dependencies."
  (interactive "sPackage to remove: ")
  (pixi--run-command (format "remove %s" package)))

(defun pixi-install ()
  "Run pixi install."
  (interactive)
  (pixi--run-command "install"))

(defun pixi-update ()
  "Run pixi update."
  (interactive)
  (pixi--run-command "update"))

(defun pixi-upgrade ()
  "Update pixi itself."
  (interactive)
  (pixi--run-command "self-update"))

(defun pixi-clean ()
  "Cleanup the pixi environments."
  (interactive)
  (pixi--run-command "clean"))

(defun pixi-workspace-channel-add (channel)
  "Add CHANNEL to the workspace."
  (interactive "sChannel to add: ")
  (pixi--run-command (format "workspace channel add %s" channel)))

(defun pixi-workspace-channel-remove (channel)
  "Remove CHANNEL from the workspace."
  (interactive "sChannel to remove: ")
  (pixi--run-command (format "workspace channel remove %s" channel)))

(defun pixi-workspace-channel-list ()
  "List channels in the workspace."
  (interactive)
  (pixi--run-command "workspace channel list"))

(defun pixi-workspace-platform-list ()
  "List platforms in the workspace."
  (interactive)
  (pixi--run-command "workspace platform list"))

(defun pixi-workspace-platform-add (platform)
  "Add PLATFORM to the workspace."
  (interactive "sPlatform to add: ")
  (pixi--run-command (format "workspace platform add %s" platform)))

(defun pixi-workspace-platform-remove (platform)
  "Remove PLATFORM from the workspace."
  (interactive "sPlatform to remove: ")
  (pixi--run-command (format "workspace platform remove %s" platform)))

(defun pixi-workspace-feature-list ()
  "List features in the workspace."
  (interactive)
  (pixi--run-command "workspace feature list"))

;;; Eglot integration

(defun pixi--restart-eglot ()
  "Restart eglot if it's running, to pick up new environment."
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p))
    (when-let ((server (eglot-current-server)))
      (eglot-reconnect server))))

(add-hook 'pixi-activate-hook #'pixi--restart-eglot)

;;; Auto-activation

(defun pixi--maybe-activate ()
  "Activate pixi environment if in a pixi project and not already active."
  (when pixi-auto-activate
    (let ((root (pixi--find-project-root)))
      (when (and root
                 (not (equal root pixi--current-project)))
        (pixi-activate root)))))

(add-hook 'find-file-hook #'pixi--maybe-activate)
(add-hook 'dired-mode-hook #'pixi--maybe-activate)

(provide 'pixi)
;;; pixi.el ends here
