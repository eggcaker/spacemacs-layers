;;; packages.el --- pelm-misc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq pelm-misc-packages
    '(
      org-gcal
      ledger-mode
      ;;discover-my-major
      ))

;; List of packages to exclude.
(setq pelm-misc-excluded-packages '())

;; For each package, define a function pelm-misc/init-<package-name>
;;
(defun pelm-misc/init-org-gcal ()
    (use-package org-gcal
      :commands (org-gcal-sync org-gcal-fetch)
      :defer t
      :init
      (setq org-gcal-dir "~/.emacs.d/.cache/org-gcal/")
      (setq org-gcal-logo "org.png")
      (setq org-gcal-token-file "~/.emacs.d/.cache/.org-gcal-token")
      (setq org-gcal-file-alist '(("eggcaker@gmail.com" .  "~/.org-files/google.org")))))


(defun pelm-misc/post-init-ledger-mode ()
  (use-package ledger-mode
    :ensure t
    :init
    (setq ledger-clear-whole-transactions 1)
    :mode "\\.dat"
    :config
    (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)))
