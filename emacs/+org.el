;;; ../.dotfiles/emacs/+org.el -*- lexical-binding: t; -*-

;; ORG MODE
;; ;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/Personal/.org/")

(progn
  (progn
    (face-spec-set 'org-level-5 ;; originally copied from org-level-8
      (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
        '((((class color) (min-colors 16) (background light)) (:foreground "brightblue"))
          (((class color) (min-colors 16) (background dark)) (:foreground "brightblue"))
          (((class color) (min-colors 8)) (:foreground "green")))))
    (face-spec-set 'org-level-6 ;; originally copied from org-level-8
      (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
        '((((class color) (min-colors 16) (background light)) (:foreground "brightcyan"))
          (((class color) (min-colors 16) (background dark)) (:foreground "brightcyan"))
          (((class color) (min-colors 8)) (:foreground "green")))))
    (face-spec-set 'org-level-7 ;; originally copied from org-level-8
      (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
        '((((class color) (min-colors 16) (background light)) (:foreground "deepskyblue"))
          (((class color) (min-colors 16) (background dark)) (:foreground "deepskyblue"))
          (((class color) (min-colors 8)) (:foreground "green")))))
    (face-spec-set 'org-level-8 ;; originally copied from org-level-8
      (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
        '((((class color) (min-colors 16) (background light)) (:foreground "Purple"))
          (((class color) (min-colors 16) (background dark)) (:foreground "Purple"))
          (((class color) (min-colors 8)) (:foreground "green")))))
    (defface org-level-9 ;; originally copied from org-level-8
      (org-compatible-face nil ;; not inheriting from outline-9 because that does not exist
        '((((class color) (min-colors 16) (background light)) (:foreground "RosyBrown"))
          (((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon"))
          (((class color) (min-colors 8)) (:foreground "green"))))
      "Face used for level 9 headlines."
      :group 'org-faces)
    (setq org-level-faces (append org-level-faces (list 'org-level-9)))
    (setq org-n-level-faces (length org-level-faces))))
;; Hide Org markup indicators
;; (after! org (setq org-hide-emphasis-markers t))
;;   (font-lock-add-keywords 'org-mode
;;                           '(("^ *\\([-]\\) "
;;                              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;   (custom-theme-set-faces
;;    'user
;;    '(variable-pitch ((t (:family "Fira Code" :height 180 :weight thin))))
;;    '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))

;;   (add-hook 'org-mode-hook 'variable-pitch-mode)

  ;; (custom-theme-set-faces
  ;;  'user
  ;;  '(org-block ((t (:inherit fixed-pitch))))
  ;;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-document-info ((t (:foreground "dark orange"))))
  ;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
  ;;  '(org-link ((t (:foreground "royal blue" :underline t))))
  ;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
  ;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  ;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  ;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;   (let* ((variable-tuple
;;           (cond ((x-list-fonts "Fira Code")         '(:font "Fira Code"))
;;                 ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                 ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                 ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                 ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                 (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;          (base-font-color     (face-foreground 'default nil 'default)) ;
;;          (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    ;; (custom-theme-set-faces
    ;;  'user
    ;;  `(org-level-8 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-7 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-6 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-5 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
    ;;  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
    ;;  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
    ;;  `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
    ;;  `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

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
(zz/add-file-keybinding "C-c z w" "~/Dropbox/Personal/.org/Work/work.org.gpg" "work.org")
(zz/add-file-keybinding "C-c z i" "~/Dropbox/Personal/.org/ideas.org" "ideas.org")
(zz/add-file-keybinding "C-c z p" "~/Dropbox/Personal/.org/projects.org" "projects.org")
(zz/add-file-keybinding "C-c z d" "~/Dropbox/Personal/.org/diary.org" "diary.org")

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

(after! org-roam
(map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-capture" "c" #'org-roam-capture))

(require 'company-org-roam)
(use-package company-org-roam
:when (featurep! :completion company)
:after org-roam
:config
(set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev)))

(use-package org-journal
:bind
("C-c n j" . org-journal-new-entry)
:custom
(org-journal-dir "~/Dropbox/Personal/.org/")
(org-journal-date-prefix "#+TITLE: ")
(org-journal-file-format "%Y-%m-%d.org")
(org-journal-date-format "%A, %d %B %Y"))
(setq org-journal-enable-agenda-integration t)
