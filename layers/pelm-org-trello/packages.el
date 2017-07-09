;;; packages.el --- pelm-org-trello layer packages file for Spacemacs.
;;
;;
;;; Code:

(defconst pelm-org-trello-packages '(org-trello))

(defvar trello-excluded-packages '())

(defun pelm-org-trello/init-org-trello ()
  (use-package org-trello
    :commands (org-trello/version
               org-trello/install-key-and-token
               org-trello/install-board-metadata
               org-trello/sync-card
               org-trello/sync-buffer
               org-trello/assign-me
               org-trello/check-setup
               org-trello/delete-setup
               org-trello/create-board-and-install-metadata
               org-trello/kill-entity
               org-trello/kill-cards
               org-trello/archive-card
               org-trello/archive-cards
               org-trello/jump-to-trello-card
               org-trello/jump-to-trello-board
               org-trello/add-card-comments
               org-trello/show-card-comments
               org-trello/show-card-labels
               org-trello/update-board-metadata
               org-trello/help-describing-bindings
               )
    :init
    ;; org-trello major mode for all .trello files
    (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))

    ;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
    (add-hook 'org-mode-hook
              (lambda ()
                (let ((filename (buffer-file-name (current-buffer))))
                  (when (and filename (string= "trello" (file-name-extension filename)))
                    (org-trello-mode)))))

    :config
    (progn
      (evil-leader/set-key
        "ots" 'org-trello/sync-buffer
        "otc" 'org-trello/sync-card)

			(defun sync-card-from-server()
				(interactive)
				(let ((current-prefix-arg 4))
					(call-interactively 'org-trello/sync-card)))

			(let ((kmap org-trello-mode-map))

				;; Evil trello keybindings
				(evil-define-key '(normal visual motion) kmap
					"oA" 'org-trello-archive-cards
					"op" 'org-trello/sync-card
					"oF" 'sync-card-from-server
					"oa" 'org-trello-toggle-assign-user
					"oc" 'org-trello-add-card-comment
					"oC" 'org-trello-sync-comment
					"oI" 'org-trello-install-board-metadata
					"oj" 'org-trello-jump-to-trello-card
					"oJ" 'org-trello-jump-to-trello-board
					"ok" 'org-trello-kill-entity
					"oK" 'org-trello-kill-cards
					"ol" 'org-trello-show-board-labels
					"os" 'org-trello-sync-buffer
					"om" 'org-trello-update-board-metadata
				))

			)))

;;; packages.el ends here
