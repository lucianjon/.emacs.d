;;; definers.el --- Main keybind definers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'general)
(require 'seq)

(defun lj-comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

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
  "wo"  '(delete-other-windows :which-key "delete other")
  "tm"  '(toggle-frame-maximized :which-key "maximise window"))

(general-create-definer lj-local-leader-def
  :states 'motion
  :keymaps 'override
  :prefix "SPC m")

(general-def 'motion
  "/" 'counsel-grep-or-swiper)

(provide 'definers)

;;; definers.el ends here
