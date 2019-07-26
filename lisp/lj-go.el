;;; lj-go.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Lucian Jones

;; Author: Lucian Jones <lucianm.jones@gmail.com>

;;; Commentary:

;;; Code:

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :preface
  (defun lj-modules-p ()
    "Return non-nil if this buffer is part of a Go Modules project."
    (locate-dominating-file default-directory "go.mod"))

  (defun lj-setup-go ()
    "Run setup for Go buffers."
    (progn
      (setq-local tab-width 4)
      (setq-local indent-tabs-mode t)
      (if (lj-modules-p)
          (setenv "GO111MODULE" "on")
        (setenv "GO111MODULE" "off"))
      (lsp)))
  :hook
  (go-mode . lj-setup-go)
  :config
  (progn
    (setq gofmt-command "goimports")
    ;; (setq godoc-at-point-function 'godoc-gogetdoc)

    (add-hook 'before-save-hook 'gofmt-before-save)

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
      "SPC mr" "refactor"
      "SPC mg" "goto"
      "SPC mh" "help"
      "SPC mt" "test")

    (lj-local-leader-def
      :keymaps 'go-mode-map
      "tp" 'lj-go-run-package-tests
      "tf" 'lj-go-run-test-current-function)))

(use-package go-rename
  :ensure t
  :after go-mode
  :config
  (lj-local-leader-def
    :keymaps 'go-mode-map
    "rn" 'go-rename))

(use-package flycheck-golangci-lint
  :ensure t
  :config
  (setq flycheck-disabled-checkers '(go-gofmt
                                     go-golint
                                     go-vet
                                     go-build
                                     go-test
                                     go-errcheck))
  :hook (go-mode . flycheck-golangci-lint-setup))

(use-package go-eldoc
  :ensure t
  :after go-mode
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(setq counsel-rg-base-command "rg -i -g '!.git/*' --no-heading --line-number --hidden --color never %s .")

(use-package go-tag
  :ensure t
  :after go-mode
  :init
  (lj-local-leader-def
    :keymaps 'go-mode-map
    "rf" 'go-tag-add
    "rF" 'go-tag-remove))

(use-package godoctor
  :ensure t
  :init
  (progn
    (lj-local-leader-def
      :keymaps 'go-mode-map
      "re" 'godoctor-extract
      "rt" 'godoctor-toggle
      "rd" 'godoctor-godoc)))

(use-package go-keyify
  :init
  (progn
    (lj-local-leader-def
      :keymaps 'go-mode-map
      "rk" 'go-keyify)))

(use-package go-guru
  :ensure t
  :after go-mode
  :init
  (which-key-add-major-mode-key-based-replacements 'go-mode "SPC mf" "guru")
  (lj-local-leader-def
    :keymaps 'go-mode-map
    "fd" 'go-guru-describe
    "ff" 'go-guru-freevars
    "fi" 'go-guru-implements
    "fc" 'go-guru-peers
    "fr" 'go-guru-referrers
    "fj" 'go-guru-definition
    "fp" 'go-guru-pointsto
    "fs" 'go-guru-callstack
    "fe" 'go-guru-whicherrs
    "f<" 'go-guru-callers
    "f>" 'go-guru-callees
    "fo" 'go-guru-set-scope))

(use-package go-fill-struct
  :ensure t
  :init
  (lj-local-leader-def
    :keymaps 'go-mode-map
    "rs" 'go-fill-struct))

(use-package go-impl
  :ensure t
  :init
  (lj-local-leader-def
    :keymaps 'go-mode-map
    "ri" 'go-impl))


(provide 'lj-go)

;;; lj-go.el ends here
