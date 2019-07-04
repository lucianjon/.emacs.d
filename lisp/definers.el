;;; definers.el --- Main keybind definers -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'general)

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
