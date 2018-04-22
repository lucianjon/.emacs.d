;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Lucian Jones

;; Author: Lucian Jones <lucianm.jones@gmail.com>

;;; Commentary:

;; Declares some variables and bootstraps the rest of the configuration.

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq gc-cons-threshold 50000000)

(setq large-file-warning-threshold 100000000)

(setq ring-bell-function 'ignore)

(setq tab-width 2)

(setq scroll-step           1
      scroll-margin         0
      scroll-conservatively 10000)

(setq make-backup-files nil)

(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(global-set-key (kbd "M-/") 'hippie-expand)

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(blink-cursor-mode 0)
(horizontal-scroll-bar-mode -1)
(save-place-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)

(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
    apropos-do-all t
    mouse-yank-at-point t
    require-final-newline t
    visible-bell t
    load-prefer-newer t
    ediff-window-setup-function 'ediff-setup-windows-plain
    save-place-file (concat user-emacs-directory "places")
    backup-directory-alist `(("." . ,(concat user-emacs-directory
                                             "backups"))))

(define-key minibuffer-local-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") #'keyboard-escape-quit)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(add-to-list 'default-frame-alist '(font . "Source Code Pro-16" ))
(set-face-attribute 'default t :font "Source Code Pro-16")

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package evil
  :ensure t
  :config
  (progn
    (setq evil-mode-line-format nil)
    (evil-mode 1)))

(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-mode)
    (setq which-key-idle-delay 0.2)))

(use-package general
  :ensure t
  :config
  (general-override-mode))

(general-create-definer lj-leader-def
  :prefix "SPC")

(general-create-definer lj-local-leader-def
  :prefix "SPC m")

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package magit
  :ensure t
  :functions (magit-display-buffer-fullframe-status-v1)
  :init
  (lj-leader-def
    :states 'normal
    :keymaps 'override
    "gs" 'magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package evil-magit
  :ensure t
  :after magit)

;; solarized-theme
(use-package solarized-theme
  :ensure t
  :config
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch nil)
  (setq solarized-scale-org-headlines nil)
  (setq solarized-high-contrast-mode-line t)
  (load-theme 'solarized-light t))

;; Mac OSX specific settings
(if (eq system-type 'darwin)
    (progn
      (use-package exec-path-from-shell
        :ensure t
        :config
        (exec-path-from-shell-initialize)
        (exec-path-from-shell-copy-env "GOPATH"))
      (setq mac-command-modifier 'meta)
      (setq mac-right-option-modifier 'control)
      (setq dired-use-ls-dired nil)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :init
  (lj-leader-def
    :states 'normal
    :keymaps 'override
      "TAB" 'ivy-switch-buffer)
  :config
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))))

(use-package counsel
  :ensure t
  :after swiper
  :init
  (lj-leader-def
    :states 'normal
    :keymaps 'override
      "SPC" 'counsel-M-x
      "ff"  'counsel-find-file)
  (general-def 'motion
    "/" 'counsel-grep-or-swiper))

(use-package projectile
  :ensure t
  :config
  (progn
    (setq projectile-completion-system 'ivy)
    (setq projectile-switch-project-action 'magit-status)
    (setq projectile-enable-caching t)
    (setq projectile-globally-ignored-files '("TAGS" ".DS_Store"))
    (setq projectile-globally-ignored-file-suffixes '("gz" "zip" "tar" "elc"))
    (setq projectile-globally-ignored-directories
          '(".bzr"
            ".ensime_cache"
            ".eunit"
            ".fslckout"
            ".g8"
            ".git"
            ".hg"
            ".idea"
            ".stack-work"
            ".svn"
            "build"
            "dist"
            "node_modules"
            "*vendor"
            "target"))
    (projectile-mode)))


(use-package counsel-projectile
  :ensure t
  :config
  (lj-leader-def
    :states 'normal
    :keymaps 'override
      "pf" 'counsel-projectile-find-file
      "pd" 'counsel-projectile-find-dir
      "pb" 'counsel-projectile-switch-to-buffer
      "pp" 'counsel-projectile-switch-project
      "/"  'counsel-projectile-ag))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :init
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t))
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)

    (defun lj-go-run-tests (args)
      (interactive)
      (save-selected-window
        (async-shell-command (concat "go test " args)))

    (defun lj-go-run-package-tests ()
      (interactive)
      (lj-go-run-tests "")))

    (lj-local-leader-def
      :states 'normal
      :keymaps 'override
      "tp" 'lj-go-run-package-tests)) ;; fix this

(use-package go-rename
  :ensure t
  :after go-mode
  :config
  (lj-local-leader-def
    :states 'normal
    :keymaps 'override
    "rn" 'go-rename))

(use-package company-go
  :after go-mode

  :config
  (progn
    (setq company-go-show-annotation t)
    (setq company-idle-delay .3)
    (setq company-echo-delay 0)
    (add-hook 'go-mode-hook
      (lambda ()
        (set (make-local-variable 'company-backends) '(company-go))
        (company-mode)))))

(use-package go-eldoc
  :ensure t
  :after go-mode
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(provide 'init)

;;; init.el ends here

