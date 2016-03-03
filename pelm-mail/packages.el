;;; packages.el --- pelm-mail layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: eggcaker <eggcaker@gmail.com>
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

(setq pelm-mail-packages '((mu4e :location site)))

(defun pelm-mail/post-init-mu4e ()
 ;; mu4e config
  (setq mu4e-account-alist
        '(("gmail"
           ;; Under each account, set the account-specific variables you want.
           (mu4e-sent-messages-behavior delete)
           (mu4e-sent-folder "/Gmail/[Gmail].Sent Mail")
           (mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
           (mu4e-trash-folder "/Gmail/[Gmail].Trash")
           (mu4e-refile-folder "/Gmail/[Gmail].Archive")

           (user-mail-address "eggcaker@gmail.com")
           (user-full-name "eggcaker"))))

  (mu4e/mail-account-reset)
  (setq mu4e-maildir "~/.mails"
        mu4e-trash-folder "/Trash"
        mail-user-agent 'mu4e-user-agent
        mu4e-refile-folder "/Archive"
        mu4e-get-mail-command "offlineimap"
        mu4e-update-interval nil
        mu4e-view-prefer-html t
        mu4e-compose-signature "eggcaker\nIt is easy to make things.\nIt is hard to make things simple."
        mu4e-html2text-command "w3m -T text/html"
        mu4e-view-show-images t
        mu4e-display-image t
        mu4e-view-show-addresses t)

  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;;; Mail directory shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/Gmail/INBOX" . ?j)
          ("/Gmail/PacerHealth" . ?p)))

  ;; Custom marks
  (setq mu4e-headers-new-mark              '("N" . "✉")
        mu4e-headers-empty-parent-prefix '("-" . "○")
        mu4e-headers-first-child-prefix '("\\" . "┗━❯")
        mu4e-headers-has-child-prefix '("+" . "┗◉")
        mu4e-headers-duplicate-prefix      '("=" . "⚌")
        mu4e-headers-default-prefix  '("|" . "┃"))


  ;;; Bookmarks
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam" "Unread messages" ?u)
          ("date:today..now AND NOT maildir:/Gmail/[Gmail].Spam" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?i)
          ("flag:unread AND NOT flag:trashed AND maildir:/Gmail/[Gmail].Spam"     "Unread spam"               ?s)

          ))

  ;; Send mail setup
  ;; SMTP setup
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        starttls-use-gnutls t)
  ;; Personal info
  (setq user-full-name "Tongzhu, Zhang")
  (setq user-mail-address "eggcaker@gmail.com")
  ;; gmail setup
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-smtp-user "eggcaker@gmail.com"))


;;; packages.el ends here
