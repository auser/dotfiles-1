;;; ../.dotfiles/emacs/+ui.el -*- lexical-binding: t; -*-

;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Auto splash screen
;;(let ((alternatives '("doom-emacs-bw-light.svg")))
;;  (setq fancy-splash-image
;;        (concat doom-private-dir "splash/"
;;                (nth (random (length alternatives)) alternatives))))

;; Base and variable pitch fonts
;; (setq doom-font (font-spec :family "Fira Code" :size 15)
;;       ;; doom-variable-pitch-font (font-spec :family "ETBembo" :size 15)
;;       )

;; Allow mixed fonts in a buffer
(add-hook! 'org-mode-hook #'mixed-pitch-mode)
(setq mixed-pitch-variable-pitch-cursor nil)
