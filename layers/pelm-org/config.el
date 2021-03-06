

;; Org non-standard faces.
(defface pelm-org-created-kwd
  '((t (:weight bold :box nil :foreground "#EF9A9A" :background nil)))
  "Face used to display state NEW.")

(defface pelm-org-in-progress-kwd
  '((t (:weight bold :box nil :foreground "#2196F3" :background nil )))
  "Face used to display state INPROGRESS.")

(defface pelm-org-waiting-for-kwd
  '((t (:weight bold :box nil :foreground "#89C58F" :background nil)))
  "Face used to display state WAIT.")

(defface pelm-org-someday-kwd
  '((t (:weight bold :box nil :foreground "#9EB6D4" :background nil)))
  "Face used to display state SOMEDAY.")

(defface pelm-org-quote-kwd
  '((t (:weight bold :box nil :foreground "#FC5158" :background nil)))
  "Face used to display .")

(defface pelm-org-quoted-kwd
  '((t (:weight bold :box nil :foreground "#55BA80" :background nil)))
  "Face used to display .")

(defface pelm-org-approved-kwd
  '((t (:weight bold :box nil :foreground "#969696" :background nil)))
  "Face used to display .")

(defface pelm-org-rejected-kwd
  '((t (:weight bold :box nil :foreground "#42B5FF" :background nil)))
  "Face used to display state REJECTED.")

(defface pelm-org-openpo-kwd
  '((t (:weight bold :box nil :foreground "#FC5158" :background nil)))
  "Face used to display OPEN purchase order.")

(defface pelm-org-closedpo-kwd
  '((t (:weight bold :box nil :foreground "#969696" :background nil)))
  "Face used to display CLOSED purchase order.")

(defvar org-agenda-dir ""
  "gtd org files location")

(defvar deft-dir ""
  "deft org files locaiton")

(setq-default
 org-agenda-dir "~/.org-notes/gtd/"
 deft-dir "~/.org-notes")

(defconst pelm-org-completed-date-regexp
  (concat "\\("
          "CLOSED: \\[%Y-%m-%d"
          "\\|"
          "- State \"\\(DONE\\|CANCELLED\\)\" * from .* \\[%Y-%m-%d"
          "\\|"
          "- State .* ->  *\"\\(DONE\\|CANCELLED\\)\" * \\[%Y-%m-%d"
          "\\)")
  "Matches any completion time stamp.")

(add-hook 'org-pomodoro-started-hook '(lambda () (interactive) (pelm-org-clock-increment-property "Pomodoro_Started")))
(add-hook 'org-pomodoro-finished-hook '(lambda () (interactive) (pelm-org-clock-increment-property "Pomodoro_Finished")))
(add-hook 'org-pomodoro-killed-hook '(lambda () (interactive) (pelm-org-clock-increment-property "Pomodoro_Killed")))

;; (add-hook 'org-after-todo-state-change-hook '(lambda () (interactive)
;;                                                (when (-contains? org-done-keywords org-state)
;;                                                  (org-pomodoro-finished))))
