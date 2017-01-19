;;; packages.el --- pelm-slack layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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

;;; Code:

(defconst pelm-slack-packages
  '(
;;    company
 ;;   company-emoji
    slack
    persp-mode
    smooth-scrolling
    ))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun pelm-slack/post-init-company ()
    (spacemacs|add-company-hook slack-mode)
    (push 'company-capf company-backends-slack-mode))

  (defun pelm-slack/post-init-company-emoji ()
    (push 'company-emoji company-backends-slack-mode)))

(defun pelm-slack/post-init-smooth-scrolling ()
  (add-hook 'slack-mode-hook 'spacemacs//unset-scroll-margin))


(defun pelm-slack/post-init-persp-mode ()
  ;; do not save slack buffers
  (with-eval-after-load 'persp-mode
    (push (lambda (b) (with-current-buffer b (eq major-mode 'slack-mode)))
          persp-filter-save-buffers-functions))

  (spacemacs|define-custom-layout slack-spacemacs-layout-name
    :binding slack-spacemacs-layout-binding
    :body
    (progn
      (defun spacemacs-layouts/add-slack-buffer-to-persp ()
        (persp-add-buffer (current-buffer)
                          (persp-get-by-name
                           slack-spacemacs-layout-name)))
      (add-hook 'slack-mode-hook #'spacemacs-layouts/add-slack-buffer-to-persp)
      (call-interactively 'slack-start))))


(defun pelm-slack/init-slack()
  (use-package slack
    :commands (slack-start)
    :init
    (setq slack-enable-emoji nil) ;; if you want to enable emoji, default nil
    (setq slack-prefer-current-team t)
    (spacemacs/set-leader-keys
      "aCs" 'slack-start
      "aCj" 'slack-channel-select
      "aCd" 'slack-im-select
      "aCq" 'slack-ws-close
      )
    :config

    (spacemacs/set-leader-keys-for-major-mode 'slack-mode
      "j" 'slack-channel-select
      "d" 'slack-im-select
      "p" 'slack-room-load-prev-messages
      "e" 'slack-message-edit
      "q" 'slack-ws-close
      "mm" 'slack-message-embed-mention
      "mc" 'slack-message-embed-channel
      "k" 'slack-channel-select
      "@" 'slack-message-embed-mention
      "#" 'slack-message-embed-channel
      )
    (evil-define-key 'insert slack-mode-map
      (kbd "@") 'slack-message-embed-mention
      (kbd "#") 'slack-message-embed-channel)
    ))


;;; packages.el ends here
