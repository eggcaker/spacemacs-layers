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
      ;;discover-my-major
      ))

;; List of packages to exclude.
(setq pelm-misc-excluded-packages '())

;; For each package, define a function pelm-misc/init-<package-name>
;;
(defun pelm-misc/init-discover-my-major ()
    (use-package discover-my-major
      :defer t
      :init
      (progn
        (evil-leader/set-key (kbd "mhm") 'discover-my-major)
        (evilify makey-key-mode makey-key-mode-get-key-map ))))
