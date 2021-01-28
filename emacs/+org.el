;;; ../.dotfiles/emacs/+org.el -*- lexical-binding: t; -*-

;; ORG MODE
;; ;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Personal/Org/")
(setq org-agenda-files '("~/Dropbox/Personal/Org/"))

(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))))

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

;; org-roam?
(setq org-roam-directory org-directory)
(setq +org-roam-open-buffer-on-find-file nil)


(after! org
  (setq org-download-method 'directory)
  (setq org-download-image-dir "images")
  (setq org-download-heading-lvl nil)
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-image-actual-width 300)
  (map! :map org-mode-map
        "C-c l a y" #'zz/org-download-paste-clipboard
        "C-M-y" #'zz/org-download-paste-clipboard))

(after! org
  (require 'org-tempo)
  (set-popup-rule! "^ \\*Org tags" :side 'bottom :size 0.80 :select t :ttl nil)

  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup)
  )
