;;; init.el --- Startup file for Emacs.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Lucian Jones

;; Author: Lucian Jones <lucianm.jones@gmail.com>

;;; Commentary:

;; Declares some variables and bootstraps the rest of the configuration.

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq-default js-indent-level 2)

(define-key minibuffer-local-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") #'keyboard-escape-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") #'keyboard-escape-quit)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.00)

;; Auto-indent on RET
(define-key global-map (kbd "RET") #'comment-indent-new-line)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)


(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; Disable backup files
(defvar backup-dir (expand-file-name "~/personal-emacs-config/backup/"))
(defvar autosave-dir (expand-file-name "~/personal-emacs-config/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq create-lockfiles nil)

(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(set-frame-font "Hack 15")

(use-package lj-basic)

(use-package all-the-icons
  :straight t)

(use-package s
  :straight t)

(autoload 'projectile-project-root "projectile")

(defconst lj-counsel--escape-characters '("$" "*")
  "Characters to escape for input into counsel.")

(defun lj-counsel--escape-character-p (char)
  "Determines if a CHAR should be escaped for input into counsel."
  (-contains-p lj-counsel--escape-characters char))

(defun lj-counsel--escape-character (char)
  "Escape a CHAR for input into counsel."
  (concat "\\" char))

(defun lj-counsel--escape-string (string)
  "Escape STRING for input into counsel."
  (-if-let* ((str-list (split-string string "" t))
             (escaped-str-list (-map-when #'lj-counsel--escape-character-p #'lj-counsel--escape-character str-list)))
      (s-join "" escaped-str-list)
    string))

(defun lj--region-or-symbol-at-pt ()
  "Get symbol at point or text in selected region."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (thing-at-point 'symbol t)))

(defun lj--region-or-symbol ()
  "Get text or symbol at point, or an empty string if neither exist."
  (if-let* ((text (lj--region-or-symbol-at-pt)))
      text
    ""))

(defun lj-swiper-region-or-symbol (input)
  "Run `swiper' with INPUT, which is either the selected region or the symbol at point."
  (interactive (list (lj--region-or-symbol)))
  (swiper input))

(defun lj-counsel-project-region-or-symbol (input)
  "Search project for INPUT, which is either the selected region or the symbol at point."
  (interactive (list (lj--region-or-symbol)))
  (counsel-rg (lj-counsel--escape-string input) (projectile-project-root)))

(defun lj-counsel-region-or-symbol (start-dir input)
  "Search START-DIR for INPUT which is either the selected region or symbol at point."
  (interactive (list (read-directory-name "Start from directory: ")
                     (lj--region-or-symbol)))
  (counsel-rg (lj-counsel--escape-string input) start-dir))

(use-package f
  :straight t)

(use-package general
  :straight t
  :config
  (general-override-mode))

(require 'definers)

(use-package doom-themes
  :straight t
  :init (load-theme 'doom-nord t)
  :config
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 15))

(use-package smex
  :straight t
  :config
  (smex-initialize))

(use-package evil
  :straight t
  :after general
  :general
  (:states '(normal motion)
           "C-u" #'evil-scroll-page-up
           "C-d" #'evil-scroll-page-down)
  (:states 'motion
           "gb" #'xref-pop-marker-stack)
  (:keymaps 'help-mode-map
            :states 'motion
            "<escape>" #'quit-window
            "<tab>" #'forward-button
            "S-<tab>" #'backward-button
            "]" #'help-go-forward
            "[" #'help-go-back
            "gf" #'help-go-forward
            "gb" #'help-go-back
            "gh" #'help-follow-symbol)
  :config
  (progn
    (setq evil-mode-line-format nil)
    (evil-mode 1)
    (setq evil-visual-state-cursor '("gray" (hbar . 2)))
    (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
    (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))))

(use-package evil-escape
  :straight t
  :after evil
  :config
  (progn
    (setq-default evil-escape-key-sequence "jk")
    (setq-default evil-escape-delay 0.07)
    (evil-escape-mode)))

(use-package which-key
  :straight t
  :config
  (progn
    (which-key-mode)
    (setq which-key-idle-delay 0.0)))

(use-package smartparens
  :straight t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode)))

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ws-butler
  :straight t
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package diff-hl
  :straight t
  :config
  (progn
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (diff-hl-mode)))

(use-package dired
  :defer t
  :config
  (progn
    (setq-default dired-listing-switches "-alhv")))

(use-package autorevert
  :straight t
  :config
  (progn
    (setq global-auto-revert-non-file-buffers t)
    (setq auto-revert-verbose nil)
    (global-auto-revert-mode 1)))

(defun lj-uuidgen-1 (arg)
  "Return a time based UUID (UUIDv1).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-1)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))

(use-package uuidgen
  :straight t
  :commands (uuidgen-1)
  :init
  (progn
    (lj-leader-def "iUU" 'lj-uuidgen-1 :which-key "generate v1 UUID")))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package posframe
  :straight t)

(use-package flycheck-posframe
  :straight t
  :after flycheck
  :custom
  (flycheck-posframe-border-width 5)
  :config
  (progn
    (flycheck-posframe-configure-pretty-defaults)
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))))

(use-package wgrep
  :straight t
  :commands (wgrep-setup)
  :init
  (add-hook 'grep-setup-hook #'wgrep-stup)
  :config
  (progn
    (setq wgrep-auto-save-buffer t)))

(use-package popwin
  :straight t
  :config (popwin-mode 1))

(use-package magit
  :straight t
  :functions (magit-display-buffer-fullframe-status-v1)
  :init
  (lj-leader-def "gs" 'magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package forge
  :straight t
  :after magit)

(use-package evil-magit
  :straight t
  :after magit)

;; solarized-theme
;; (use-package solarized-theme
;;   :straight t
;;   :config
;;   (progn
;;     (setq solarized-distinct-fringe-background t)
;;     (setq solarized-use-variable-pitch nil)
;;     (setq solarized-scale-org-headlines nil)
;;     (setq solarized-high-contrast-mode-line t)
;;     (load-theme 'solarized-light t)))
;;

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :straight t
  :init
  (exec-path-from-shell-copy-env "GO111MODULE")
  (exec-path-from-shell-copy-env "GOPRIVATE")
  (exec-path-from-shell-initialize))

;; Mac OSX specific settings
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . 'nil))
      (add-to-list 'default-frame-alist
                   '(font . "Source Code Pro 16"))
      (setq mac-command-modifier 'meta)
      (setq mac-right-option-modifier 'control)
      (setq dired-use-ls-dired nil)
      (setq frame-title-format nil)))

(use-package ivy
  :straight t
  :diminish (ivy-mode . "")
  :config
  (progn
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
    (setq ivy-height 10)
    (setq ivy-initial-inputs-alist nil)))

(use-package counsel
  :straight t
  :after swiper)

(use-package flx-ido
  :straight t
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

(use-package projectile
  :straight t
  :init
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
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
  :straight t
  :config
  (setq counsel-projectile-switch-project-action 'magit-status)
  (lj-leader-def
    "pf" 'counsel-projectile-find-file
    "pd" 'counsel-projectile-find-dir
    "pb" 'counsel-projectile-switch-to-buffer
    "pp" 'counsel-projectile-switch-project
    "/"  'counsel-projectile-rg))

(use-package lj-ivy-commands
  :after ivy
  :commands (lj-swiper-region-or-symbol
             lj-counsel-project-region-or-symbol
             lj-counsel-region-or-symbol)
  :init
  (lj-leader-def
    "sS" '(lj-swiper-region-or-symbol :wk "search in buffer")
    "sP" '(lj-counsel-project-region-or-symbol :wk "search in project")
    "sF" '(lj-counsel-region-or-symbol :wk "search in dir")))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package lj-company)
(use-package lj-lsp)
(use-package lj-go)

(use-package graphql-mode
  :defer t
  :mode ("\\.graphql\\'" . graphql-mode))

(use-package protobuf-mode
  :defer t
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package terraform-mode
  :defer t
  :mode ("\\.tf\\'" . terraform-mode)
  :config
  (add-hook #'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package dockerfile-mode
  :defer t
  :mode ("\\Dockerfile\\'" . dockerfile-mode))

(use-package diminish
  :straight t)

(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package lj-hydra)

(use-package json-mode
  :defer t
  :mode ("\\.json\\'" . json-mode))

(use-package scala-mode
  :defer t
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :defer t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package lsp-scala
  :defer t
  :after scala-mode
  :hook (scala-mode . lsp))

(use-package jsonnet-mode
  :straight t
  :mode "\\.jsonnet$")

(use-package deadgrep
  :straight t
  :commands (deadgrep)
  :general (:keymaps 'deadgrep-mode-map "C-c C-w" #'deadgrep-edit-mode)

  :preface
  (defun config-editing--deadgrep-requery ()
    (interactive)
    (let ((button (save-excursion
                    (goto-char (point-min))
                    (forward-button 1))))
      (button-activate button)))
  :general (:states 'normal :keymaps 'deadgrep-mode-map "c" #'config-editing--deadgrep-requery)

  :preface
  (defun config-editing--on-enter-deadgrep-edit-mode (&rest _)
    (message "Entering edit mode. Changes will be made to underlying files as you edit."))
  :config
  (advice-add #'deadgrep-edit-mode :after #'config-editing--on-enter-deadgrep-edit-mode)

  :preface
  (defun config-editing--on-exit-deadgrep-edit-mode (&rest _)
    (when (derived-mode-p 'deadgrep-edit-mode)
      (message "Exiting edit mode.")))
  :config
  (advice-add #'deadgrep-mode :before #'config-editing--on-exit-deadgrep-edit-mode))

(use-package lj-python)

(use-package typescript-mode
  :straight t
  :mode ("\\.tsx?\\'" . typescript-mode)
  :custom
  (typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode-hook #'lsp))

(use-package yaml-mode
  :straight t
  :mode "\\.y\\(aml\\|ml\\)$")

(use-package lj-sql)

(setq gc-cons-threshold 100000000) ;; 100mb
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(provide 'init)

;;; init.el ends here

