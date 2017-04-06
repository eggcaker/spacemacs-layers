;;; packages.el --- pelm-misc Layer packages File for Spacemacs

(setq pelm-misc-packages
    '(
      (kotlin-mode  :location local)
      ledger-mode
      evil-lion
      ;;discover-my-major
      ))

(defun pelm-misc/init-evil-lion()
  (use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode)))

(defun pelm-misc/post-init-ledger-mode ()
  (use-package ledger-mode
    :ensure t
    :init
    (spacemacs/set-leader-keys "ga" 'pelm/add-ssh-key)
    (setq ledger-clear-whole-transactions 1)
    :mode "\\.dat"
    :config
    (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)))

(defun pelm-misc/init-kotlin-mode()
  (use-package kotlin-mode
    :init
    (setq kotlin-tab-width 2 )
    :mode (("\\.kt\\'" . kotlin-mode))))
