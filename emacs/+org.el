;;; ../.dotfiles/emacs/+org.el -*- lexical-binding: t; -*-

;; ORG MODE
;; ;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.org/")

;; Hide Org markup indicators
(after! org (setq org-hide-emphasis-markers t))
;; Insert ORG headings at point (not after current subtree)
(after! org (setq org-insert-heading-respect-content nil))
;; Enable logging of done tasks, log stuff into the LOGBOOK drawer
(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t))

;; Use the special C-a, C-e and C-k definitions for Org,
;; which enable some special behavior in headings.
(after! org
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t))

;; disable electric mode
;;(add-hook! org-mode (electric-indent-local-mode -1))

;; Enable variable and visual line mode in Org mode by default.
(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode)

;; I define some global keybindings to open my frequently-used org files
;; (original tip from Learn how to take notes more efficiently in Org Mode).
;; First, I define a helper function to define keybindings that open files.
;; Note that this requires lexical binding to be enabled, so that the lambda
;; creates a closure, otherwise the keybindings don’t work.
(defun zz/add-file-keybinding (key file &optional desc)
  (let ((key key)
        (file file)
        (desc desc))
    (map! :desc (or desc file)
          key
          (lambda () (interactive) (find-file file)))))

;; keybindings for commonly used org files
(zz/add-file-keybinding "C-c z w" "~/.org/Work/work.org.gpg" "work.org")
(zz/add-file-keybinding "C-c z i" "~/.org/ideas.org" "ideas.org")
(zz/add-file-keybinding "C-c z p" "~/.org/projects.org" "projects.org")
(zz/add-file-keybinding "C-c z d" "~/.org/diary.org" "diary.org")

;; org-roam?
(setq org-roam-directory org-directory)
(setq +org-roam-open-buffer-on-find-file nil)

;; Capturing images
(defun zz/org-download-paste-clipboard (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if (not use-default-filename)
             (read-string (format "Filename [%s]: "
                                  org-download-screenshot-basename)
                          nil nil org-download-screenshot-basename)
           nil)))
    (org-download-clipboard file)))

(after! org
  (setq org-download-method 'directory)
  (setq org-download-image-dir "images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width 300)
  (map! :map org-mode-map
        "C-c l a y" #'zz/org-download-paste-clipboard
        "C-M-y" #'zz/org-download-paste-clipboard))
