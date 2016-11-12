;;; packages.el --- typing-games Layer packages File for Spacemacs
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

(defvar typing-games-packages '(speed-type typing))

(defun typing-games/init-speed-type ()
  "Initialize my package"
  (use-package speed-type
    :defer t)
  )
(defun typing-games/init-typing ()
  "Initialize my package"
  (use-package typing
    :defer t)
  )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
