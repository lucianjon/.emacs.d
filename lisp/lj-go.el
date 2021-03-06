;;; lj-go.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Lucian Jones

;; Author: Lucian Jones <lucianm.jones@gmail.com>

;;; Commentary:

;;; Code:

(use-package go-mode
  :straight t
  :defer t
  :mode ("\\.go\\'" . go-mode)
  :preface
  (defun lj-setup-go ()
    "Run setup for Go buffers."
    (progn
      (setq-local tab-width 4)
      (setq-local indent-tabs-mode t)))
  :hook
  (go-mode . lj-setup-go)
  :config
  (progn
    (add-hook 'before-save-hook 'lsp-organize-imports)
    (add-hook 'before-save-hook 'lsp-format-buffer)

    ;; TODO: make this function a little nicer
    (defun lj-go-run-tests (args)
      (interactive)
      (if (file-exists-p "Makefile")
          (let ((make-args (if (string= "" args)
                               ""
                             (concat "GOTEST_ARGS=" args))))
            (compilation-start (concat "make test" " " make-args " " "")
                               nil (lambda (n) "*go test*") nil))
        (compilation-start (concat "go test" args " " "")
                           nil (lambda (n) "*go test*") nil)))

    (defun lj-go-run-package-tests ()
      (interactive)
      (lj-go-run-tests ""))

    (defun lj-go-run-test-current-function ()
      (interactive)
      (if (string-match "_test\\.go" buffer-file-name)
          (save-excursion
            (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
            (lj-go-run-tests (concat " -run" "='" (match-string-no-properties 2) "$'")))
        (message "Must be in a _test.go file to run go-run-test-current-function")))

    (push '("*go test*" :dedicated t :position bottom :stick t :noselect t :height 0.25) popwin:special-display-config)

    ;; TODO: integrate with general definer
    (which-key-add-major-mode-key-based-replacements 'go-mode
      "SPC mt" "test")

    (lj-local-leader-def
      :keymaps 'go-mode-map
      "tp" 'lj-go-run-package-tests
      "tf" 'lj-go-run-test-current-function)))

(use-package go-eldoc
  :straight t
  :defer t
  :after go-mode
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(provide 'lj-go)

;;; lj-go.el ends here
