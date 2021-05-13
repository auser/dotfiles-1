;;; ../Development/personal/dotfiles/emacs/+python.el -*- lexical-binding: t; -*-


(use-package elpy
    :init
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    :bind (:map elpy-mode-map
	      ("<M-left>" . nil)
	      ("<M-right>" . nil)
	      ("<M-S-left>" . elpy-nav-indent-shift-left)
	      ("<M-S-right>" . elpy-nav-indent-shift-right)
	      ("M-." . elpy-goto-definition)
	      ("M-," . pop-tag-mark))
    :config
    (setq elpy-rpc-backend "jedi"))

(use-package python
  :mode ("\\.py" . python-mode)
  :config
  (setq python-indent-offset 4)
  (elpy-enable))


(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-c p e" . pyenv-activate-current-project))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyenv-mode-set pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))

(defvar pyenv-current-version nil nil)

(defun pyenv-init()
  "Initialize pyenv's current version to the global one."
  (let ((global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global"))))
    (message (concat "Setting pyenv version to " global-pyenv))
    (pyenv-mode-set global-pyenv)
    (setq pyenv-current-version global-pyenv)))

(add-hook 'after-init-hook 'pyenv-init)
