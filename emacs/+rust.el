;;; ../Development/personal/dotfiles/emacs/+rust.el -*- lexical-binding: t; -*-

(custom-set-faces
  '(rustic-compilation-column ((t (:inherit compilation-column-number))))
  '(rustic-compilation-line ((t (:foreground "LimeGreen")))))

(setq rustic-lsp-server 'rls)

;(setq rustic-lsp-client 'eglot)
;
(with-eval-after-load "lsp-rust"
 (lsp-register-client
  (make-lsp-client
   :new-connection (lsp-stdio-connection
                    (lambda ()
                      `(,(or (executable-find
                              (cl-first lsp-rust-analyzer-server-command))
                             (lsp-package-path 'rust-analyzer)
                             "rust-analyzer")
                        ,@(cl-rest lsp-rust-analyzer-server-args))))
   :remote? t
   :major-modes '(rust-mode rustic-mode)
   :initialization-options 'lsp-rust-analyzer--make-init-options
   :notification-handlers (ht<-alist lsp-rust-notification-handlers)
   :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
   :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
   :after-open-fn (lambda ()
                    (when lsp-rust-analyzer-server-display-inlay-hints
                      (lsp-rust-analyzer-inlay-hints-mode)))
   :ignore-messages nil
   :server-id 'rust-analyzer-remote)))

