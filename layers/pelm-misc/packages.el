;;; packages.el --- pelm-misc Layer packages File for Spacemacs

(setq pelm-misc-packages
    '(
      ledger-mode
      ;;discover-my-major
      ))

(defun pelm-misc/post-init-ledger-mode ()
  (use-package ledger-mode
    :ensure t
    :init
    (spacemacs/set-leader-keys "ga" 'pelm/add-ssh-key)
    (setq ledger-clear-whole-transactions 1)
    :mode "\\.dat"
    :config
    (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)))

