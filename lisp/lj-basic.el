;;; lj-basic.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Lucian Jones

;; Author: Lucian Jones <lucianm.jones@gmail.com>

;;; Commentary:

;;; Code:

(setq gc-cons-threshold 50000000)

(setq large-file-warning-threshold 100000000)

(setq ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)

(setq user-full-name "Lucian Jones")
(setq user-mail-address "lucianm.jones@gmail.com")

(setq compilation-scroll-output t)
(set-fringe-mode 0)
(set-face-attribute 'mode-line nil :box nil :overline nil)

(setq scroll-step           1
      scroll-margin         0
      scroll-conservatively 10000)

(setq initial-scratch-message nil
      inhibit-splash-screen t
      inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(blink-cursor-mode 0)
(horizontal-scroll-bar-mode -1)
(save-place-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq-default fill-column 80)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(global-auto-revert-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(setq save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(provide 'lj-basic)

;;; lj-basic.el ends here
