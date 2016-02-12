;;; packages.el --- stack-exchange Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 eggcaker
;;
;; Author: eggcaker<eggcaker@gmail.com>
;; URL: https://github.com/eggcaker
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

(setq stack-exchange-packages '(sx))

;; List of packages to exclude.
(setq stack-exchange-excluded-packages '())


(defun stack-exchange/init-sx()
  (use-package sx
    :commands (sx-tab-all-questions
               sx-tab-unanswered
               sx-tab-unanswered-my-tags
               sx-tab-featured
               sx-tab-starred)
    :init
    (progn
      (evil-leader/set-key
        "sxa" 'sx-tab-all-questions
        ))
    :config
    (progn


      )))


;;; packages.el ends here
