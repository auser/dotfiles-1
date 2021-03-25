;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; patch to emacs@28.0.50
;; https://www.reddit.com/r/emacs/comments/kqd9wi/changes_in_emacshead2828050_break_many_packages/
(defmacro define-obsolete-function-alias ( obsolete-name current-name
                                           &optional when docstring)
  "Set OBSOLETE-NAME's function definition to CURRENT-NAME and mark it obsolete.
\(define-obsolete-function-alias \\='old-fun \\='new-fun \"22.1\" \"old-fun's doc.\")
is equivalent to the following two lines of code:
\(defalias \\='old-fun \\='new-fun \"old-fun's doc.\")
\(make-obsolete \\='old-fun \\='new-fun \"22.1\")
WHEN should be a string indicating when the function was first
made obsolete, for example a date or a release number.
See the docstrings of `defalias' and `make-obsolete' for more details."
  (declare (doc-string 4)
           (advertised-calling-convention
           ;; New code should always provide the `when' argument
           (obsolete-name current-name when &optional docstring) "23.1"))
  `(progn
     (defalias ,obsolete-name ,current-name ,docstring)
     (make-obsolete ,obsolete-name ,current-name ,when)))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ari Lerner"
      user-mail-address "me@ari.io")


;; Setup exec-path
;;; add to ~/.doom.d/config.el
(require 'exec-path-from-shell)
(when (display-graphic-p)
  (exec-path-from-shell-initialize))

(setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
(setq exec-path (append exec-path '("/usr/bin")))

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;;
;;
;; Change Mac modifiers
;; (cond (IS-MAC
;;        (setq mac-command-modifier 'meta
;;              mac-option-modifier 'alt
;;              mac-right-option-modifier 'alt)))

;; Remove the whole line instead of just emptying it
(setq kill-whole-line t)

;; There are two ways to load a theme. Both assume the theme is installed and


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Enable auto-save and backup files
(setq auto-save-default t
      make-backup-files t)

;; Disable exit confirmation
(setq confirm-kill-emacs nil)

;;
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;

;; See recently opened files
;; (map! "C-x b"   #'counsel-buffer-or-recentf
;;       "C-x C-b" #'counsel-switch-buffer)


(load! "+ui")
(load! "+org")
(load! "+web")
