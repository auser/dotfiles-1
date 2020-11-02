(defun write-window-setup ()
(interactive)
	(split-window-right)
	(windmove-right)
	(split-window-below)
	(windmove-left)
	(find-file "*draft.org" t)
	(windmove-right)
	(find-file "*notes.md" t)
	(windmove-left))

;; (with-eval-after-load 'dired
;;   (define-key dired-mode-map [f3] #'write-window-setup))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Basics

(global-visual-line-mode 1)

;; - - -
;; Modeline
(use-package doom-modeline
  :config (progn
            (require 'doom-modeline-segments)
            (doom-modeline-def-segment conda-env
              "The current conda environment.  Works with `conda'."
              (when (bound-and-true-p conda-env-current-name)
                (propertize (format " |%s|" conda-env-current-name)
                            'face (if (doom-modeline--active) 'mode-line 'mode-line-inactive)
                            'help-echo (format "Conda environment: %s"
                                               conda-env-current-name))))
            (setq doom-modeline-icon t
                  doom-modeline-major-mode-icon t
                  doom-modeline-major-mode-color-icon t
                  doom-modeline-buffer-file-name-style 'truncate-upto-project
                  doom-modeline-buffer-state-icon t
                  doom-modeline-github nil
                  doom-modeline-buffer-encoding nil
                  doom-modeline-minor-modes nil)

            (doom-modeline-def-modeline 'main
              '(bar workspace-name window-number modals
                    matches buffer-info remote-host
                    buffer-position word-count parrot selection-info
                    conda-env)
              '(objed-state misc-info persp-name battery grip irc mu4e
                            gnus github debug lsp minor-modes input-method
                            indent-info buffer-encoding major-mode process vcs checker))
            (if (bound-and-true-p imenu-list-mode-line-format)
                (setq imenu-list-mode-line-format
                      '((:eval
                         (doom-modeline-segment--bar))
                        (:propertize "%b" face mode-line-buffer-id)
                        " "
                        (:eval (buffer-name imenu-list--displayed-buffer))
                        " "
                        (:eval
                         (doom-modeline-segment--matches)))))
            (add-hook 'mu4e-compose-mode-hook #'doom-modeline-set-minimal-modeline)))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Fonts
(use-package unicode-fonts)

; Redefine some EVIL keys back to normal Emacs keys for more fluid editing

;; (with-eval-after-load 'evil-maps
;; 	(define-key evil-motion-state-map (kbd "C-a") 'move-beginning-of-line)
;; 	(define-key evil-motion-state-map (kbd "C-e") 'move-end-of-line)
;; 	(define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
;; 	(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line))

; If you want to redefine more, here are their names
; https://github.com/magnars/.emacs.d/blob/master/site-lisp/evil/evil-maps.el


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Mac Specific

;; Uncomment if colours look off
;;(setq ns-use-srgb-colorspace nil)


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Spellcheck Setup

;; (with-eval-after-load "ispell"
;; 	(setq ispell-program-name "aspell")
;; 	(ispell-set-spellchecker-params)
;; 	(setq ispell-dictionary "en_GB"))



;; ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; ;; Terminal Environment

;; (defun my-shell-setup ()
;; 	(interactive)
;; 	(setq buffer-face-mode-face '(:family "PxPlus IBM VGA8" :height 160))
;; 	(buffer-face-mode))
;; (add-hook 'term-mode-hook #'my-shell-setup)



;; ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; ;; Image Environment

;; (defun set-image-borderless ()
;; 	(setq left-margin-width 0)
;; 	(setq right-margin-width 0)
;; 	(set-fringe-mode 0)
;; 	(setq global-hl-line-mode nil)
;; 	(image-transform-fit-to-height))
;; (add-hook 'image-mode-hook #'set-image-borderless)



;; ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


;; (defun my-toggle-margins-low ()
;; "Set margins in current buffer for use in narrow windows."
;; (interactive)
;;   (if (or (> left-margin-width 0) (> right-margin-width 0))
;;     (progn
;;       (setq left-margin-width 0)
;;       (setq right-margin-width 0)
;;       (set-fringe-mode nil)
;;       (setq global-hl-line-mode t)
;;       (set-window-buffer (selected-window) (current-buffer)))
;;     (setq left-margin-width 5)
;;     (setq right-margin-width 5)
;;     (set-fringe-mode 0)
;;     (setq global-hl-line-mode nil)
;;     (set-window-buffer (selected-window) (current-buffer))))

;; (global-set-key [f5] #'my-toggle-margins-low)


;; (defun my-toggle-margins-wide ()
;; "Set margins in current buffer for use in full-screen."
;; (interactive)
;;   (if (or (> left-margin-width 0) (> right-margin-width 0))
;;     (progn
;;       (setq left-margin-width 0)
;;       (setq right-margin-width 0)
;;       (set-fringe-mode nil)
;;       (setq global-hl-line-mode t)
;;       (set-window-buffer (selected-window) (current-buffer)))
;;     (setq left-margin-width 80)
;;     (setq right-margin-width 80)
;;     (set-fringe-mode 0)
;;     (setq global-hl-line-mode nil)
;;     (set-window-buffer (selected-window) (current-buffer))))

;; (global-set-key [f8] #'my-toggle-margins-wide)



;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Projectile

;; (setq projectile-completion-system 'ivy)
;; (setq projectile-ignore-global '(".DS_Store" ".gitmodules" ".gitignore"))
;(setq projectile-project-search-path '("~/work/"))



;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; Markdown

(defun markdown-setup ()
	(interactive)
	(visual-line-mode 1)
	(setq buffer-face-mode-face '(:family "SF Mono" :height 200 :foreground "#aba7a0"))
	(buffer-face-mode)
	(setq line-spacing 3)
	(setq left-margin-width 8)
	(setq right-margin-width 8)
	(flyspell-mode 1)
	(setq global-hl-line-mode nil)
	(setq header-line-format " "))
(add-hook 'markdown-mode-hook #'markdown-setup)
(add-hook 'markdown-mode-hook 'prettify-symbols-mode)


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; ;; Org-Mode

;; (defun split-and-indirect-orgtree ()
;; "Splits window to the right and opens an org tree section in it"
;; (interactive)
;; (split-window-right)
;; (org-tree-to-indirect-buffer)
;; (windmove-right))


;; (defun kill-and-unsplit-orgtree ()
;; "Kills the cloned buffer and deletes the window."
;; (interactive)
;; (kill-this-buffer)
;; (delete-window))


;; (with-eval-after-load 'org
;; 	;(setq org-agenda-files '("~/Org/Notes/"
;; 	;                         "~/Org/Agenda/"))
;; 	;(setq org-default-notes-file "~/Org/Notes/notes.org")
;; 	(setq org-ellipsis "⤵")
;; 	(setq org-catch-invisible-edits 'show-and-error)
;; 	(setq org-hide-emphasis-markers t)
;; 	(setq org-fontify-whole-heading-line t)
;; 	(setq org-tags-column 0)
;; 	(setq org-bullets-bullet-list '("⬢" "◆" "▲" "■"))
;; 	(setq org-adapt-indentation t)
;; 	(setq line-move-visual t)
;; 	; Change Default Keymaps
;; 	(define-key org-mode-map (kbd "C-S-<return>") 'org-insert-subheading)
;; 	; Shortcuts to Interactive Functions
;; 	(define-key org-mode-map [f9]  #'split-and-indirect-orgtree)
;; 	(define-key org-mode-map [f12] #'kill-and-unsplit-orgtree)
;; 	(define-key org-mode-map [f7]  #'org-html-export-to-html))


;; ; Things we can't set as defaults above, we can set here
;; (defun org-setup ()
;; 	(setq line-spacing 3)
;; 	(flyspell-mode 1)
;; 	(setq global-hl-line-mode nil)
;; 	(set-fringe-mode 0)
;; 	(setq left-margin-width 5)
;; 	(setq right-margin-width 5)
;; 	(setq header-line-format " ")
;; 	(olivetti-mode 1))
;; (add-hook 'org-mode-hook #'org-setup)

;; ;; The Hooks! Might be faster to set this as separate hooks instead of one big function
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
;; (add-hook 'org-mode-hook 'visual-line-mode)
;; (add-hook 'org-mode-hook 'org-indent-mode)
;; (add-hook 'org-mode-hook 'prettify-symbols-mode)


;; (defun my-org-config/setup-buffer-face ()
;; 	(setq buffer-face-mode-face '(:family "SF Mono"))
;; 	(buffer-face-mode)
;; )
;; (add-hook 'org-agenda-mode-hook 'my-org-config/setup-buffer-face)

;; (custom-theme-set-faces
;; 	'user
;; 	'(variable-pitch ((t (:family "SF Mono" :height 200 :foreground "#a5967e"))))
;; 	'(fixed-pitch ((t (:family "SF Mono" :height 180 ))))
;; 	'(flyspell-incorrect ((t (:foreground "#d3ebe9" :background "#c23127"))))
;; 	'(header-line ((t (:background "#1c1e1f" :height 220))))
;; 	'(org-document-title        ((t (:foreground "#f2f2f2" :weight bold :height 400))))
;; 	'(org-meta-line             ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; 	'(org-document-info         ((t (:foreground "#51c4b5"))))
;; 	'(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
;; 	'(org-block                 ((t (:inherit fixed-pitch))))
;; 	'(org-link                  ((t (:foreground "royal blue" :underline t))))
;; 	'(org-property-value        ((t (:inherit fixed-pitch))) t)
;; 	'(org-special-keyword       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
;; 	'(org-tag                   ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
;; 	'(org-verbatim              ((t (:inherit (shadow fixed-pitch)))))
;; 	'(org-indent                ((t (:inherit (org-hide fixed-pitch)))))
;; 	'(org-level-1               ((t (:foreground "#ffaf69"))))
;; 	'(org-level-2               ((t (:foreground "#3fc6b7"))))
;; 	'(org-level-3               ((t (:foreground "#dc4d59"))))
;; 	'(org-list-dt               ((t (:foreground "#ea412b"))))
;; 	'(org-table                 ((t (:inherit fixed-pitch))) t)
;; 	'(org-ellipsis              ((t (:foreground "#51c4b5")))))


;; ;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; ;; Writing Window Setup

;; (defun write-window-setup ()
;; (interactive)
;; 	(split-window-right)
;; 	(windmove-right)
;; 	(split-window-below)
;; 	(windmove-left)
;; 	(find-file "*draft.org" t)
;; 	(windmove-right)
;; 	(find-file "*notes.md" t)
;; 	(windmove-left))

;; (with-eval-after-load 'dired
;; 	(define-key dired-mode-map [f3] #'write-window-setup))


;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "cf3d5d77679f7daed6a2c863e4f2e30427d5e375b254252127be9359957502ec" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" default))
 '(org-todo-keywords
   '((sequence "TODO(t)" "PROJ(p)" "STRT(s)" "WAIT(w)" "HOLD(h)" "|" "DONE(d)" "KILL(k)" "INPROGRESS(i)" "TOREAD(r)")
     (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")))
 '(package-selected-packages
   '(lsp-mode dockerfile-mode docker jetbrains-darcula-theme impatient-mode ox-pandoc tide xah-css-mode emmet-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-tab ((t (:foreground "#636363")))))
