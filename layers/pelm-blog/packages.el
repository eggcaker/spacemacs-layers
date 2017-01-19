;;; packages.el --- Blog Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 eggcaker
;;
;; Author: eggcaker<eggcaker@gmail.com>
;; URL: https://github.com/eggcaker
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(setq pelm-blog-packages '(org))

(defun pelm-blog/post-init-org()
  (use-package org
    :commands (pelm/create-blog-post)
    :init
    (progn

      (defun pelm/create-blog-post ()
         (interactive "P")
         (message "create new post" nil)
         )

      (evil-leader/set-key "aop" 'pelm/create-blog-post))
      ))

;;; packages.el ends here
