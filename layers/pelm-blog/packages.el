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

(defconst pelm-blog-packages '(blog-admin))

(defun pelm-blog/init-blog-admin()
(use-package blog-admin
  :init
  (progn
    (setq blog-admin-backend-path "~/src/personal/yep8.org")
    (setq blog-admin-backend-type 'hexo)
    (setq blog-admin-backend-new-post-in-drafts t) ;; create new post in drafts by default
    (setq blog-admin-backend-new-post-with-same-name-dir t) ;; create same-name directory with new post
    (setq blog-admin-backend-hexo-config-file "_config.yml") ;; default assumes _config.yml
    )))

;;; packages.el ends here
