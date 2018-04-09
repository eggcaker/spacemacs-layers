;;; packages.el --- pelm-kotlin layer packages file for Spacemacs.
;;
;; Copyright (c) 2017-2017 eggcaker
;;
;; Author: eggcaker <eggcaker@gmail.com>
;; URL: https://github.com/spacemacs-layers/
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Commentary:

;;; Code:

(defconst pelm-kotlin-packages '(kotlin-mode ob-kotlin))
(defun pelm-kotlin/init-ob-kotlin()
  (use-package ob-kotlin
    :ensure t
    :defer t))

(defun pelm-kotlin/post-init-kotlin-mode()
  (use-package kotlin-mode
    :defer t
    :init
    (setq  kotlin-tab-width 2)
    :mode (("\\.kt[s]\\'" . kotlin-mode))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'kotlin-mode
        "sr" 'kotlin-send-region
        "ss" 'kotlin-send-line
        "sb" 'kotlin-send-buffer
        "sc" 'kotlin-send-block
        "r" 'kotlin-repl
        "f" 'c-indent-line-or-region
        ))))

;;; packages.el ends here
