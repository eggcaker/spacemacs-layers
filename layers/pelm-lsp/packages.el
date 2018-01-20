;;; packages.el --- pelm-lsp layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Tongzhu, Zhang <eggcaker@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst pelm-lsp-packages 
  '((lsp-mode :location local)
    (lsp-ui :location local)
    company-lsp
    (cquery :location local)
    helm-xref
    markdown-mode
    ))

(defun pelm-lsp/init-lsp-mode ()
  (use-package lsp-mode
    :config (add-hook  'prog-major-mode #'lsp-prog-major-mode-enable)))

(defun pelm-lsp/post-init-markdown-mode ()
  (use-package markdown-mode)) ;; no defer

(defun pelm-lsp/init-lsp-ui ()
  (use-package lsp-ui
    :after lsp-mode
    :after markdown-mode
    :config (add-hook 'lsp-after-open-hook 'lsp-ui-mode)
    ))

(defun pelm-lsp/init-company-lsp ()
  (use-package company-lsp
    :init (push 'company-lsp company-backends)))

(defun pelm-lsp/init-helm-xref ()
  "from github.com/MaskRay/Config"
  (use-package helm-xref
    :config
    (progn
      (setq xref-prompt-for-identifier
            '(not xref-find-definitions xref-find-definitions-other-window
                  xref-find-definitions-other-frame xref-find-references
                  spacemacs/jump-to-definition spacemacs/jump-to-reference))
      (setq xref-show-xrefs-function 'helm-xref-show-xrefs))))

(defun pelm-lsp/init-cquery ()
  (use-package cquery
    :init
    (progn
      (spacemacs/add-to-hooks #'lsp-cquery-enable '(c-mode-hook c++-mode-hook))
      (dolist (mode '(c-mode c++-mode))
        (evil-leader/set-key-for-mode mode
          "r." 'xref-find-definitions
          "r," 'xref-find-references
          "r[" 'xref-pop-marker-stack
          "rl" 'helm-imenu
          )))))

;;; packages.el ends here
