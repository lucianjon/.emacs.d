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

(setq load-prefer-newer t)

(setq gc-cons-threshold 50000000)

(setq large-file-warning-threshold 100000000)

(setq ring-bell-function 'ignore)

(setq tab-width 2)

(setq indent-tabs-mode nil)

(setq scroll-step           1
      scroll-conservatively 10000)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(blink-cursor-mode 0)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package evil
  :ensure t
  :config
  (evil-mode 1)
)


(use-package general
  :ensure t
  :config
  (general-override-mode)
)

(use-package magit
  :ensure t)

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
        (exec-path-from-shell-initialize))
      (setq mac-command-modifier 'meta)
      (setq mac-right-option-modifier 'control)
      ;; macOS ls doesn't support --dired
      (setq dired-use-ls-dired nil)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
	;; allow input not in order
        '((t . ivy--regex-ignore-order))))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x))

(use-package projectile
  :ensure t)

(general-create-definer lj-leader-def
  :prefix "SPC")

(general-create-definer lj-local-leader-def
  :prefix "SPC m")

(use-package counsel-projectile
  :ensure t
  :init
  (lj-leader-def
    :states 'normal
    :keymaps 'override
      "pf" 'counsel-projectile-find-file
      "pd" 'counsel-projectile-find-dir
      "pb" 'counsel-projectile-switch-to-buffer
      "pp" 'counsel-projectile-switch-project
      "/"  'counsel-projectile-ag))

(lj-leader-def
  :states 'normal
  :keymaps 'override
    "SPC" 'counsel-M-x
    "TAB" 'ivy-switch-buffer
    "/" 'counsel-ag
    "ff" 'counsel-find-file)

(provide 'init)

;;; init.el ends here

