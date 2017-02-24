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

(defconst pelm-blog-packages '(org-page blog-admin))

(defun pelm-blog/init-org-page()
    (spacemacs/declare-prefix "ab" "blog")
    (use-package org-page
      :config (progn (setq op/repository-directory "~/src/personal/blog/"
                             op/site-main-title "简"
                           op/site-sub-title "大道至简"
                           op/site-domain "http://yep8.org"
                           op/theme-root-directory "~/src/geek/org-page/themes"
                           op/theme 'kd_mdo
                           op/personal-github-link "http://github.com/eggcaker"
                           op/personal-google-analytics-id "")
                     (spacemacs/set-leader-keys
                       "abp" 'op/do-publication-and-preview-site
                       "abP" 'op/do-publication
                       "abn" 'op/new-post))))

(defun pelm-blog/init-blog-admin()
(use-package blog-admin
  :init
  (progn

    (setq blog-admin-backend-type 'org-page)
    (setq blog-admin-backend-path "~/src/personal/blog")
    (setq blog-admin-backend-new-post-in-drafts t)
    (setq blog-admin-backend-new-post-with-same-name-dir t)
    (setq blog-admin-backend-org-page-drafts "_drafts") ;; directory to save draft
    (setq blog-admin-backend-org-page-config-file nil) ;; if nil init.el is used


    ;; (setq blog-admin-backend-path "~/src/personal/yep8.org")
    ;; (setq blog-admin-backend-type 'hexo)
    ;; (setq blog-admin-backend-new-post-in-drafts t) ;; create new post in drafts by default
    ;; (setq blog-admin-backend-new-post-with-same-name-dir t) ;; create same-name directory with new post
    ;; (setq blog-admin-backend-hexo-config-file "_config.yml") ;; default assumes _config.yml

    )))

;;; packages.el ends here
