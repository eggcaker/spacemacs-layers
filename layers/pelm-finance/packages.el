;;; packages.el --- pelm-finance layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq pelm-finance-packages
  '(
    company
    (flycheck-ledger :toggle (configuration-layer/package-usedp 'flycheck))
    hledger-mode
    ))

(defun pelm-finance/post-init-company ()
  (spacemacs|add-company-backends
    :backends company-capf
    :modes hledger-mode))

(defun pelm-finance/init-flycheck-ledger ()
  (with-eval-after-load 'flycheck
    (require 'flycheck-ledger)))

(defun pelm-finance/init-hledger-mode ()
  (use-package hledger-mode
    :after htmlize
    :mode ("\\.\\(hledger\\|journal\\)\\'" . hledger-mode)
    :defer t
    :commands hledger-enable-reporting
    :init
    (progn
      (setq hledger-jfile
            (expand-file-name "~/.finance/accounting.journal")
            hledger-email-secrets-file "~/.email.el")
      ;; (setq hledger-post-amount-alignment-column 62)
      ;; (spacemacs/set-leader-keys-for-major-mode 'hledger-mode
      ;;    "hd" 'hledger-delete-current-transaction
      ;;    "a" 'hledger-add-transaction
      ;;    "b" 'hledger-post-edit-amount
      ;;    "c" 'hledger-toggle-current
      ;;    "C" 'hledger-mode-clean-buffer
      ;;    "l" 'hledger-display-hledger-stats
      ;;    "p" 'hledger-display-balance-at-point
      ;;    "q" 'hledger-post-align-xact
      ;;    "r" 'hledger-reconcile
      ;;    "R" 'hledger-report
      ;;    "t" 'hledger-insert-effective-date
      ;;    "y" 'hledger-set-year
      ;;    "RET" 'hledger-set-month)
      ;; (spacemacs/set-leader-keys-for-major-mode 'hledger-reconcile-mode
      ;;   (or dotspacemacs-major-mode-leader-key ",") 'hledger-reconcile-toggle
      ;;   "a" 'hledger-reconcile-add
      ;;   "q" 'hledger-reconcile-quit
      ;;   "t" 'hledger-reconcile-change-target
      ;;   "RET" 'hledger-reconcile-finish)
      ;; TODO remove this hack if the limitation is removed upstream

      )))
