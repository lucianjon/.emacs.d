;;; lj-python.el --- Configuration for Python.  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Raghuvir Kasturi

;; Author: Raghuvir Kasturi <raghuvir.kasturi@gmail.com>

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'use-package))

(require 'dash)
(require 'definers)

(use-package python
  :after lsp-mode
  :general
  (:keymaps 'python-mode-map
            "<backspace>" nil
            "DEL" nil)
  :init
  (setq lsp-pyls-plugins-pylint-enabled nil)
  :hook (python-mode . lsp))

(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode))

(use-package pyvenv
  :straight t
  :commands (pyvenv-activate pyvenv-deactivate pyvenv-workon)
  :hook (python-mode . lj-py/pyvenv-activate-if-found)
  :preface
  (progn
    (autoload 'projectile-project-p "projectile")
    (autoload 'f-join "f")

    (defvar lj-py/venv-names '(".env" "env" ".venv" "venv" ".virtualenv"))

    (defun lj-py/directory-first-ancestor (dir pred)
      "Search up the filesystem for the first DIR satisfying PRED.
Return the first non-nil result of evalutating PRED."
      (let (result)
        (while dir
          (pcase (funcall pred dir)
            (`nil
             (setq dir (f-parent dir)))
            (res
             (setq result res)
             (setq dir nil))))
        result))

    (defun lj-py/find-venv-in-directory (dir)
      (-when-let ((dir) (--keep (let ((dir (f-join dir it)))
                                  (when (f-directory? dir)
                                    dir))
                                lj-py/venv-names))
        (file-truename dir)))

    (defun lj-py/pyvenv-dir ()
      (lj-py/directory-first-ancestor default-directory
                                      #'lj-py/find-venv-in-directory))

    (defun lj-py/pyvenv-activate-if-found ()
      (-when-let (env (lj-py/pyvenv-dir))
        (pyvenv-activate env)
        (message "Using pyvenv at %s" (f-abbrev env))))

    (defun lj-py/pyvenv-init (env)
      (interactive
       (list (or (lj-py/pyvenv-dir)
                 (f-join (read-directory-name "Project root: " nil nil t) ".env"))))
      (when (f-dir? env)
        (user-error "Environment already exists"))
      (let ((reporter (make-progress-reporter "Initializing pyvenv environment...")))
        (pcase (call-process "pyvenv" nil nil nil env)
          (`0
           (progress-reporter-update reporter)
           (pyvenv-activate env)
           (progress-reporter-done reporter))
          (_
           (message "%sFAILED" (aref (cdr reporter) 3)))))))
  :config
  (lj-local-leader-def :keymaps 'python-mode-map
    "e" '(lj-py/pyvenv-init :wk "init pyvenv")))

(provide 'lj-python)

;;; lj-python.el ends here
