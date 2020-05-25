;;; lj-lsp.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Lucian Jones

;; Author: Lucian Jones <lucianm.jones@gmail.com>

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :straight t
  :defer t
  :commands (lsp lsp-deferred lsp-prefer-flymake)
  :init
  (setq flymake-fringe-indicator-position 'right-fringe)
  :hook ((go-mode . lsp)
         (php-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :general
  (:keymaps 'lsp-mode-map :states '(normal motion visual)
            "K" #'lsp-describe-thing-at-point)
  :config
  (which-key-add-major-mode-key-based-replacements 'lsp-mode
    "SPC ml" "lsp")
  (setq lsp-prefer-flymake nil)
  (setq lsp-prefer-capf t)

  (lj-local-leader-def
    :keymaps 'lsp-mode-map
    "lg" 'lsp-find-definition
    "lr" 'lsp-find-references
    "lf" 'lsp-rename
    "lm" 'lsp-ui-imenu))

(use-package lsp-ui
  :straight t
  :defer t
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :init (setq lsp-ui-doc-enable nil
              lsp-ui-doc-header t
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'top
              lsp-ui-doc-use-webkit t
              lsp-ui-doc-border (face-foreground 'default)

              lsp-ui-sideline-enable nil
              lsp-ui-sideline-ignore-duplicate t)
  :config
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  (add-hook 'go-mode-hook 'flycheck-mode))

(use-package lsp-ivy :straight t :defer t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :straight t :defer t :commands lsp-treemacs-errors-list)

(provide 'lj-lsp)

;;; lj-lsp.el ends here
