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

(setq compilation-scroll-output t)
(set-fringe-mode 0)

(setq scroll-step           1
      scroll-margin         0
      scroll-conservatively 10000)

(setq initial-scratch-message nil)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(blink-cursor-mode 0)
(horizontal-scroll-bar-mode -1)
(save-place-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq-default fill-column 80)
(add-hook 'after-save-hook
		  'executable-make-buffer-file-executable-if-script-p)
(global-auto-revert-mode t)

(defun lj-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(require 'seq)

(defun lj-alternate-buffer (&optional window) ;; TODO: look into evil-alternate-buffer
  "Toggle back and forth between two buffers.
WINDOW sets the window in which to toggle, and defaults to the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window))
        (buffer-predicate (frame-parameter (window-frame window) 'buffer-predicate)))
    ;; switch to first buffer previously shown in this window that matches
    ;; frame-parameter `buffer-predicate'
    (switch-to-buffer
     (or (car (seq-filter (lambda (buffer)
                            (and (not (eq buffer current-buffer))
                                 (or (null buffer-predicate) (funcall buffer-predicate buffer))))
                          (seq-map #'car (window-prev-buffers window))))
         ;; `other-buffer' honors `buffer-predicate' so no need to filter
         (other-buffer current-buffer t)))))

(require 's)

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq-default indent-tabs-mode nil)
(setq save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
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

;; Instantly display current keystrokes in mini buffer
(setq echo-keystrokes 0.00)

;; Auto-indent on RET
(define-key global-map (kbd "RET") #'comment-indent-new-line)

;; Disable backup files
(setq make-backup-files nil)

(add-to-list 'load-path (concat user-emacs-directory "config"))
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(set-frame-font "Fira Code 12")

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package all-the-icons
  :ensure t)

(use-package s
  :ensure t)

(use-package f
  :ensure t)

(use-package doom-modeline
  :config
  :config
  (+doom-modeline|init))

(use-package smex
  :ensure t
  :config
  (smex-initialize))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(use-package evil
  :ensure t
  :config
  (progn
    (setq evil-mode-line-format nil)
    (evil-mode 1)
    (setq evil-visual-state-cursor '("gray" (hbar . 2)))
    (setq evil-normal-state-cursor '("DarkGoldenrod2" box))
    (setq evil-insert-state-cursor '("chartreuse3" (bar . 2)))))

(use-package evil-escape
  :ensure t
  :after evil
  :config
  (progn
    (setq-default evil-escape-key-sequence "jk")
    (evil-escape-mode)))

(use-package which-key
  :ensure t
  :config
  (progn
    (which-key-mode)
    (setq which-key-idle-delay 0.0)))

(use-package smartparens
  :ensure t
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package ws-butler
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(use-package diff-hl
  :ensure t
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
  :ensure t
  :config
  (progn
    (setq global-auto-revert-non-file-buffers t)
    (setq auto-revert-verbose nil)
    (global-auto-revert-mode 1)))

(use-package general
  :ensure t
  :config
  (general-override-mode))

(general-create-definer lj-leader-def
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :keymaps 'override
  :non-normal-prefix "M-SPC")

(lj-leader-def
  "TAB" '(lj-alternate-buffer :which-key "alternate buffer")
  "bb"  '(ivy-switch-buffer :which-key "prev buffer")
  "SPC" '(counsel-M-x :which-key "M-x")
  ";"   '(lj-comment-or-uncomment-region-or-line :which-key "comment")
  "ff"  '(counsel-find-file :which-key "find file")

  "wd"  '(evil-window-next :which-key "next window")
  "w/"  '(evil-window-vsplit :which-key "vertical split window")
  "wc"  '(evil-window-delete :which-key "delete window")
  "tm"  '(toggle-frame-maximized :which-key "maximise window"))

(general-create-definer lj-local-leader-def
  :states 'motion
  :keymaps 'override
  :prefix "SPC m")

(general-def 'motion
  "/" 'counsel-grep-or-swiper)

(defun lj-uuidgen-1 (arg)
  "Return a time based UUID (UUIDv1).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-1)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))

(use-package uuidgen
  :ensure t
  :commands (uuidgen-1)
  :init
  (progn
    (lj-leader-def "iUU" 'lj-uuidgen-1 :which-key "generate v1 UUID")))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package popwin
  :ensure t
  :config (popwin-mode 1))

(use-package magit
  :ensure t
  :functions (magit-display-buffer-fullframe-status-v1)
  :init
  (lj-leader-def "gs" 'magit-status)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package evil-magit
  :ensure t
  :after magit)

;; solarized-theme
(use-package solarized-theme
  :ensure t
  :config
  (progn
    (setq solarized-distinct-fringe-background t)
    (setq solarized-use-variable-pitch nil)
    (setq solarized-scale-org-headlines nil)
    (setq solarized-high-contrast-mode-line t)
    (load-theme 'solarized-light t)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; Mac OSX specific settings
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . 'nil))
      (set-frame-font "Source Code Pro 16")
      (setq mac-command-modifier 'meta)
      (setq mac-right-option-modifier 'control)
      (setq dired-use-ls-dired nil)
      (setq frame-title-format nil)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :config
  (progn
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-height 10)
    (setq ivy-count-format "")
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-re-builders-alist
          '((t . ivy--regex-ignore-order)))))

(use-package counsel
  :ensure t
  :after swiper)

(use-package flx-ido
  :ensure t
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

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
    "sP" 'lj-counsel-project-region-or-symbol
    "pf" 'counsel-projectile-find-file
    "pd" 'counsel-projectile-find-dir
    "pb" 'counsel-projectile-switch-to-buffer
    "pp" 'counsel-projectile-switch-project
    "/"  'counsel-projectile-ag))

(use-package go-mode
  :ensure t
  :mode ("\\.go\\'" . go-mode)
  :config
  (progn
    (setq gofmt-command "goimports")
    (setq-local tab-width 4)
    (setq-local indent-tabs-mode t)

    (add-hook 'before-save-hook 'gofmt-before-save)

    (defun lj-go-run-tests (args)
      (interactive)
      (compilation-start (concat "go test" args " " "")
                         nil (lambda (n) "*go test*") nil))

    (defun lj-go-run-package-tests ()
      (interactive)
      (lj-go-run-tests ""))

    (push '("*go test*" :dedicated t :position bottom :stick t :noselect t :height 0.25) popwin:special-display-config)

    ;; TODO: integrate with general definer
    (which-key-add-major-mode-key-based-replacements 'go-mode
      "SPC mr" "refactor"
      "SPC mg" "goto"
      "SPC mh" "help"
      "SPC mt" "test")

    (lj-local-leader-def
      :keymaps 'go-mode-map
      "gg" 'godef-jump
      "tp" 'lj-go-run-package-tests)))

(use-package go-rename
  :ensure t
  :after go-mode
  :config
  (lj-local-leader-def
    :keymaps 'go-mode-map
    "rn" 'go-rename))

(use-package company-go
  :after go-mode
  :config
  (progn
    (setq company-go-show-annotation t)
    (setq company-idle-delay .2)
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

(use-package hydra
  :ensure t)

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff)         ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)        ;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)        ;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay)   ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file)      ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(define-key dired-mode-map "." 'hydra-dired/body)

(use-package protobuf-mode
  :ensure t)

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode))

(use-package company-php
  :ensure t
  :init
  (progn
    (add-hook 'php-mode-hook 'ac-php-core-eldoc-setup)
    (add-hook 'php-mode-hook
			  (lambda ()
				(set (make-local-variable 'company-backends) '(company-php))
				(company-mode)))))

(provide 'init)

;;; init.el ends here

