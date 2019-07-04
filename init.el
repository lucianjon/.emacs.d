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

(setq-default indent-tabs-mode nil)

(setq-default js-indent-level 2)

(setq user-full-name "Lucian Jones")
(setq user-mail-address "lucianm.jones@gmail.com")

(setq compilation-scroll-output t)
(set-fringe-mode 0)
(set-face-attribute 'mode-line nil :box nil :overline nil)

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
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
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

(set-frame-font "Hack 12")

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
  :ensure t)

(use-package general
  :ensure t
  :config
  (general-override-mode))

(require 'definers)

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

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
  :ensure t
  :after evil
  :config
  (progn
    (setq-default evil-escape-key-sequence "jk")
    (setq-default evil-escape-delay 0.07)
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

(use-package highlight-thing
  :ensure t
  :commands (highlight-thing-mode)
  :init
  (add-hook 'prog-mode-hook #'highlight-thing-mode)
  :preface
  (progn

    (defun lj-highlight-thing--face-ancestors (face)
      (let (result)
        (while (and face (not (equal face 'unspecified)))
          (setq result (cons face result))
          (setq face (face-attribute face :inherit)))
        (nreverse result)))

    (defun lj-highlight-thing--should-highlight-p (res)
      "Do not highlight symbol if looking at certain faces."
      (when res
        (let ((excluded-faces '(font-lock-string-face
                                font-lock-keyword-face
                                font-lock-comment-face
                                font-lock-preprocessor-face
                                font-lock-builtin-face))
              (faces (seq-mapcat #'lj-highlight-thing--face-ancestors (face-at-point nil t))))
          (null (seq-intersection faces excluded-faces))))))

  :config
  (progn
    (setq highlight-thing-what-thing 'symbol)
    (setq highlight-thing-delay-seconds 0.5)
    (setq highlight-thing-limit-to-defun nil)
    (setq highlight-thing-case-sensitive-p t)

    (advice-add 'highlight-thing-should-highlight-p :filter-return
                #'lj-highlight-thing--should-highlight-p)))

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

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package wgrep
  :ensure t
  :commands (wgrep-setup)
  :init
  (add-hook 'grep-setup-hook #'wgrep-stup)
  :config
  (progn
    (setq wgrep-auto-save-buffer t)))

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
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "JAVA_HOME")
  (exec-path-from-shell-copy-env "PATH"))

;; Mac OSX specific settings
(if (eq system-type 'darwin)
    (progn
      (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
      (add-to-list 'default-frame-alist '(ns-appearance . 'nil))
      (add-to-list 'default-frame-alist
                   '(font . "Source Code Pro 15"))
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
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-re-builders-alist '((t . ivy--regex-plus)))
    (setq ivy-height 10)
    (setq ivy-initial-inputs-alist nil)))

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
  :ensure t
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
  :ensure t
  :config
  (yas-global-mode 1))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq flymake-fringe-indicator-position 'right-fringe)
  :hook (go-mode . lsp)
  :general
  (:keymaps 'lsp-mode-map :states '(normal motion visual)
            "K" #'lsp-describe-thing-at-point)
  :config
  (which-key-add-major-mode-key-based-replacements 'lsp-mode
    "SPC ml" "lsp")

  (lj-local-leader-def
    :keymaps 'lsp-mode-map
    "lg" 'lsp-find-definition
    "lr" 'lsp-find-references
    "lm" 'lsp-ui-imenu))

(use-package lsp-ui
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
    (setq mode-line-format nil)))

(use-package company
  :hook (after-init . global-company-mode)
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :config
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2               ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
  :general
  (:keymaps 'company-active-map
            "TAB" #'company-complete-selection
            "<tab>" #'company-complete-selection
            "S-<return>" #'company-complete-selection))

(use-package company-lsp
  :ensure t
  :after (company lsp-mode)
  :defines company-lsp
  :preface
  (progn
    (defun lj-company--lsp-mode-p ()
      (and (bound-and-true-p lsp-mode)
           (bound-and-true-p company-mode)))
    (defun lj-company--setup-lsp-backend ()
      (when (lj-company--lsp-mode-p)
        (set (make-local-variable 'company-backends) '(company-lsp)))))
  :config
  (add-hook 'company-mode-hook #'lj-company--setup-lsp-backend))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)
  ;; Support `company-common'
  (defun my-company-box--make-line (candidate)
    (-let* (((candidate annotation len-c len-a backend) candidate)
            (color (company-box--get-color backend))
            ((c-color a-color i-color s-color) (company-box--resolve-colors color))
            (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
            (candidate-string (concat (propertize company-common 'face 'company-tooltip-common)
                                      (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
            (align-string (when annotation
                            (concat " " (and company-tooltip-align-annotations
                                             (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
            (space company-box--space)
            (icon-p company-box-enable-icon)
            (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
            (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                            (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                          (company-box--apply-color icon-string i-color)
                          (company-box--apply-color candidate-string c-color)
                          align-string
                          (company-box--apply-color annotation-string a-color)))
            (len (length line)))
      (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                       'company-box--color s-color)
                           line)
      line))
  (advice-add #'company-box--make-line :override #'my-company-box--make-line)

  ;; Prettify icons
  (defun my-company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))
  (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
            (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
            (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))))))

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
      (if (lj-modules-p)
          (setenv "GO111MODULE" "on")
        (setenv "GO111MODULE" "auto"))
      (lsp)))
  :hook
  (go-mode . lj-setup-go)
  :config
  (progn
    (setq gofmt-command "goimports")
    ;; (setq godoc-at-point-function 'godoc-gogetdoc)
    (setq-local tab-width 4)
    (setq-local indent-tabs-mode t)

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
            (lj-go-run-tests (concat "-run" "='" (match-string-no-properties 2) "$'")))
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

(use-package protobuf-mode
  :ensure t)

(use-package scala-mode
  :ensure t
  :mode ("\\.scala\\'" . scala-mode))


(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'" . terraform-mode))

(use-package dockerfile-mode
  :ensure t
  :mode ("\\Dockerfile\\'" . dockerfile-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package lj-hydra)

(defvar php--lsp-ip-cmd `("node" ,(executable-find "intelephense") "--stdio"))

(use-package php-mode
  :ensure t
  :mode ("\\.php\\'" . php-mode)
  :general
  (:keymaps 'php-mode-map
            "(" nil
            "{" nil)
  :custom
  (php-template-compatibility nil)
  :preface
  (defun php--setup-intelephense ()
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection php--lsp-ip-cmd)
                      :major-modes '(php-mode)
                      :priority -1
                      :server-id 'php-ip
                      :ignore-messages '("indexing\\(Started\\|Ended\\)"))))
  (defun php--setup ()
    (php--setup-intelephense)
    (lsp))
  :hook
  (php-mode . php--setup))

(use-package css-mode
  :ensure t
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :ensure t
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; New `less-cs-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode
    :ensure t))

;; CSS eldoc
(use-package css-eldoc
  :ensure t
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

;; JSON mode
(use-package json-mode
  :ensure t)

;; Improved JavaScript editing mode
(use-package js2-mode
  :ensure t
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config
  ;; Use default keybindings for lsp
  (unbind-key "M-." js2-mode-map))

(with-eval-after-load 'flycheck
  (if (or (executable-find "eslint_d")
          (executable-find "eslint")
          (executable-find "jshint"))
      (setq js2-mode-show-strict-warnings nil))
  (if (executable-find "eslint_d")
      ;; https://github.com/mantoni/eslint_d.js
      ;; npm -i -g eslint_d
      (setq flycheck-javascript-eslint-executable "eslint_d")))

(use-package js2-refactor
  :ensure t
  :diminish js2-refactor-mode
  :hook (js2-mode . js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c C-m"))

;; Major mode for editing web templates
(use-package web-mode
  :ensure t
  :defines company-backends
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :ensure t
  :diminish skewer-mode
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode))
  :init
  ;; diminish
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))

;; Format HTML, CSS and JavaScript/JSON by js-beautify
;; Insta;; npm -g install js-beautify
(use-package web-beautify
  :ensure t
  :init
  (with-eval-after-load 'js-mode
    (bind-key "C-c b" #'web-beautify-js js-mode-map))
  (with-eval-after-load 'js2-mode
    (bind-key "C-c b" #'web-beautify-js js2-mode-map))
  (with-eval-after-load 'json-mode
    (bind-key "C-c b" #'web-beautify-js json-mode-map))
  (with-eval-after-load 'web-mode
    (bind-key "C-c b" #'web-beautify-html web-mode-map))
  (with-eval-after-load 'sgml-mode
    (bind-key "C-c b" #'web-beautify-html html-mode-map))
  (with-eval-after-load 'css-mode
    (bind-key "C-c b" #'web-beautify-css css-mode-map))
  :config
  ;; Set indent size to 2
  (setq web-beautify-args '("-s" "2" "-f" "-")))

(use-package haml-mode
  :ensure t)

(use-package scala-mode
  :ensure t
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package lsp-scala
  :ensure t
  :after scala-mode
  :demand t
  ;; Optional - enable lsp-scala automatically in scala files
  :hook (scala-mode . lsp))

(use-package deadgrep
  :ensure t
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

(provide 'init)

;;; init.el ends here

