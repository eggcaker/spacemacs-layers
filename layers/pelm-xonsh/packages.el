;;; packages.el --- pelm-xonsh layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Tongzhu, Zhang <eggcaker@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `pelm-xonsh-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `pelm-xonsh/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `pelm-xonsh/pre-init-PACKAGE' and/or
;;   `pelm-xonsh/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst pelm-xonsh-packages
  '(
    (xonsh-mode :location (recipe :fetcher github :repo "eggcaker/xonsh-mode"))))


(defun pelm-xonsh/init-xonsh-mode()
  (use-package xonsh-mode
    :defer t))

;;; packages.el ends here
