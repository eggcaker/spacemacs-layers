;;; packages.el --- pelm-org Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst pelm-org-packages '(org org-ac gnuplot ox-reveal) )
(defvar pelm-org-excluded-packages '())

(defun pelm-org/init-org-ac()
  (use-package org-ac
    :defer t
    :config
    (progn
      (org-ac/config-default))))

(defun pelm-org/post-init-org ()
  ;;   "Initialize my package"
  (use-package org
    :mode ("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)
    :defer t
    :commands (org-mode
               org-edit-src-exit
               org-agenda
               org-capture
               pelm-org/punch-in
               pelm-org/punch-out
               org-store-link
               org-agenda
               org-iswitchb
               org-clock-goto
               org-clock-in
               pelm-org/org-todo
               pelm-org/widen
               pelm-org/clock-in-last-task)
    :init
    (progn
      (evil-leader/set-key "m'" 'org-edit-src-exit)
      (global-set-key
       (kbd "<f6>") (kbd "C-c a f ."))

      ;; Display the hotlist.
      (global-set-key (kbd "<f7>") (kbd "C-c a f h"))

      ;; Display calendar for 7 days.
      (global-set-key (kbd "<f8>") (kbd "C-c a r c 7"))


      ;; set org agenda global
      (spacemacs/declare-prefix "o" "org")
      (spacemacs/set-leader-keys
        "oo" 'org-agenda
        "ob" 'org-iswitchb
        "og" 'org-clock-goto
        "oc" 'org-capture
        "os" 'org-search-view
        "oI" 'pelm-org/punch-in
        "oO" 'pelm-org/punch-out))
    :config
    (progn
      ;; set org specific keybindings
      ;; (add-hook 'org-agenda-mode-hook
      ;;           '(lambda () (org-defkey org-agenda-mode-map "R" 'org-agenda-refile))
      ;;           'append)
      ;; (add-hook 'org-agenda-mode-hook
      ;;           '(lambda () (org-defkey org-agenda-mode-map "j" 'org-agenda-next-line))
      ;;           'append)

      ;; (add-hook 'org-agenda-mode-hook
      ;;           '(lambda () (org-defkey org-agenda-mode-map "k" 'org-agenda-previous-line))
      ;;           'append)
      ;; (add-hook 'org-agenda-mode-hook
      ;;           '(lambda () (org-defkey org-agenda-mode-map (kbd "SPC") spacemacs-default-map ))
      ;;           'append)

      (defvar pelm-org/keep-clock-running nil)

      (defun update-results ()
        (let ((name (org-element-property :name (org-element-at-point)))
              (results)
              (begin))
          (when name
            (setq results
                  (save-excursion
                    (goto-char (org-babel-find-named-result name))
                    (forward-line)
                    (buffer-substring
                     (point) (org-element-property :end (org-element-at-point)))))
            (save-excursion
              (goto-char (point-min))
              (while (setq begin (org-babel-find-named-result name (point)))
                (goto-char begin)
                (forward-line)
                (setf (buffer-substring
                       (point)
                       (org-element-property :end (org-element-at-point)))
                      results))))))

      (add-hook 'org-babel-after-execute-hook 'update-results)

      (defun pelm-org/org-update-buffer-before-save ()
        "Update all dynamic blocks and all tables in the buffer before save."
        (when (derived-mode-p 'org-mode)
          (message "INFO- Update Org buffer %s"
                   (file-name-nondirectory (buffer-file-name)))
          ;; (sit-for 1.5)
          (let ((cache-long-scans nil)      ; Make `forward-line' much faster and
                                        ; thus `org-goto-line', `org-table-sum',
                                        ; etc.
                (fly-state (and (boundp 'flyspell-mode)
                                (if flyspell-mode 1 -1)))
                (buffer-undo-list buffer-undo-list)) ; For goto-chg.
            (and fly-state (flyspell-mode -1))
                                        ; Temporarily disable Flyspell to avoid
                                        ; checking the following modifications
                                        ; of the buffer.
            (measure-time "Realigned all tags" (org-align-all-tags))
            (measure-time "Updated all dynamic blocks" (org-update-all-dblocks))
            (measure-time "Re-applied formulas to all tables"
                          (org-table-iterate-buffer-tables))
            (when (file-exists-p (buffer-file-name (current-buffer)))
              (leuven-org-remove-redundant-tags))
            (and fly-state (flyspell-mode fly-state)))))

      ;; Make sure that all dynamic blocks and all tables are always up-to-date.
      (add-hook 'before-save-hook #'pelm-org/org-update-buffer-before-save)


      (eval-after-load 'org-indent
        '(spacemacs|hide-lighter org-indent-mode))

      (let ((dir (configuration-layer/get-layer-property 'pelm-org :dir)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))

      (defvar org-gtd-other-files '("~/src/personal/yep8.org/blog/index.org"))
      (setf org-agenda-files (cons "~/.org-files" org-gtd-other-files))

      ;; auto save org files
      (run-at-time "00:55" 3600 'org-save-all-org-buffers)

      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))


      ;; setq options
      (setq
       ;; mobile org options
       org-directory "~/.org-files"
       org-default-notes-files (list (concat org-directory "/refile.org"))
       org-mobile-directory "~/Dropbox/org"
       org-mobile-refile-for-pull "~/.org-files/refile.org"

       org-export-backends '(ascii beamer html latex md rss reveal)
       org-show-entry-below (quote ((default)))
       org-startup-indented t
       org-todo-repeat-to-state "TODO"
       org-clock-persist-file (concat spacemacs-cache-directory "org-clock-save.el")
       org-startup-with-inline-images t
       org-src-fontify-natively t
       org-ellipsis "⤵"
       org-ditaa-jar-path "~/.emacs.d/private/pelm-org/vendor/ditaa.jar"
       org-plantuml-jar-path "~/.emacs.d/private/pelm-org/vendor/plantuml.jar"
       org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))

       org-log-done (quote time)
       org-log-into-drawer "LOGBOOK"
       org-alphabetical-lists t
       org-agenda-span 'day
       org-src-fontify-natively t
       org-use-fast-todo-selection t
       org-refile-use-cache t
       org-treat-S-cursor-todo-selection-as-state-change nil
       org-habit-preceding-days 7
       org-habit-graph-column 110
       org-tags-column -110
       org-habit-following-days 1
       org-habit-show-habits-only-for-today t
       org-habit-show-all-today t
       org-refile-use-outline-path nil
       org-outline-path-complete-in-steps nil
       org-refile-allow-creating-parent-nodes (quote confirm) ;
       org-completion-use-ido t
       ido-everywhere t
       org-agenda-todo-ignore-with-date nil
       org-agenda-todo-ignore-deadlines nil
       org-agenda-todo-ignore-scheduled nil
       org-agenda-todo-ignore-timestamp nil
       org-agenda-skip-deadline-if-done t
       org-agenda-skip-scheduled-if-done t
       org-agenda-skip-timestamp-if-done t
       org-remove-highlights-with-change nil
       org-tags-match-list-sublevels t
       org-agenda-persistent-filter t
       org-agenda-skip-additional-timestamps-same-entry t
       org-clone-delete-id t
       org-agenda-window-setup 'current-window
       org-src-preserve-indentation nil
       org-html-coding-system 'utf-8
       org-html-head-include-default-style nil
       org-html-head-include-scripts nil
       org-tags-exclude-from-inheritance (quote ("crypt"))
       org-startup-folded 'content
       ido-max-directory-size 100000
       org-clock-in-switch-to-state 'pelm-org/clock-in-to-next
       pelm-org-diary-file "~/.org-files/diary.org"
       pelm-org-note-file "~/.org-files/notes.org"
       pelm-org-work-file "~/.org-files/work.org"
       pelm-org-life-file "~/.org-files/life.org"

       org-list-demote-modify-bullet (quote (("+" . "-")
                                             ("*" . "-")
                                             ("1." . "-")
                                             ("1)" . "-")))
       org-refile-targets (quote ((nil :maxlevel . 4)
                                  (org-agenda-files :maxlevel . 4))))

      ;; List of TODO entry keyword sequences (+ fast access keys and specifiers
      ;; for state change logging).
      (setq org-todo-keywords
            '(
              (sequence "NEW(n!)"        ; Proposal, idea (under review), to be prioritized.
                        "TODO(t!)"       ; Open, not yet started
                        "INPROGRESS(i!)" ; In progress, working on
                        "WAIT(w@/!)"     ; On hold , to be discussed, feedback
                        "SOMEDAY(m!)"    ; Someday, maybe perhaps.
                        "|"
                        "DONE(d!/!)"     ; Completed, closed fixed, verified
                        "CANX(x!)")      ; Wontfix, rejected, ignored, cancelled

              (sequence "QTE(q!)"        ; Planning.
                        "QTD(Q!)"        ; Awaiting approval.
                        "|"
                        "APP(A!)"        ; Approved.
                        "REJ(R!)")       ; Rejected.

              (sequence "OPENPO(O!)"
                        "|"
                        "CLSDPO(C!)"))

            org-todo-keyword-faces
            '(("NEW" . pelm-org-created-kwd)
              ("TODO" . org-todo)
              ("INPROGRESS" . pelm-org-in-progress-kwd)
              ("WAIT" . pelm-org-waiting-for-kwd)
              ("SOMEDAY" . pelm-org-someday-kwd)
              ("DONE" . org-done)
              ("CANX" . org-done)

              ("QTE" . pelm-org-quote-kwd)
              ("QTD" . pelm-org-quoted-kwd)
              ("APP" . pelm-org-approved-kwd)
              ("REJ" . pelm-org-rejected-kwd)

              ("OPENPO" . pelm-org-openpo-kwd)
              ("CLSDPO" . pelm-org-closedpo-kwd))


            ;; state change trigger
            ;; FIXME: need enable this agagin
            ;;org-todo-state-tags-triggers
            ;;(quote (("CANCELLED" ("CANCELLED" . t))
            ;; ("WAITING" ("WAITING" . t))
            ;; ("HOLD" ("WAITING" . t) ("HOLD" . t))
            ;; (done ("WAITING") ("HOLD"))
            ;; ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
            ;; ("INPROGRESS" ("WAITING") ("CANCELLED") ("HOLD"))
            ;; ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))
            )

      ;; Org standard faces.
      (set-face-attribute 'org-todo nil
                          :weight 'bold :box nil :foreground "#F44336" :background nil)

      (set-face-attribute 'org-done nil
                          :weight 'bold :box nil :foreground "#4CAF50" :background nil)


      (setq org-stuck-projects
            '("+LEVEL=2/-DONE"             ; Identify a project.
              ("TODO" "INPROGRESS" "NEXT") ; Todo keywords.
              nil ""))                     ; Tags, regexp.

      (setq org-capture-templates nil)

      (add-to-list 'org-capture-templates
                   `("t" "Task" entry
                     (file+headline ,(concat org-directory "/refile.org") "Tasks")
                     "* NEW %^{Task}%?

%i"
                     :empty-lines 1) t)

      (add-to-list 'org-capture-templates
                   `("T" "Task in current file" entry
                     (file+headline
                      (buffer-file-name (org-capture-get :original-buffer))
                      "Tasks")
                     "* TODO %?
  %U %a %n"
                     :prepend t) t)

      (add-to-list 'org-capture-templates
                   `("a" "Appt" entry
                     (file+headline ,(concat org-directory "/refile.org") "Events")
                     "* %^{Appointment}%?
%^T

%i"
                     :empty-lines 1) t)

      (add-to-list 'org-capture-templates
                   `("x" "CLI TODO" entry
                     (file (concat org-directory "/refile.org")) "* TODO %i\n%U" :immediate-finish t) t)

      (add-to-list 'org-capture-templates
                   `("p" "Web Notes" entry (file+headline (concat org-directory "/notes.org") "Web Notes")
                     "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?" :empty-lines 1) t)

      (add-to-list 'org-capture-templates
                   `("L" "Protocol Link" entry (file+headline (concat org-directory "/bookmarks.org") "Links")
                     "* %? [[%:link][%:description]] \nCaptured On: %U") t)

      (add-to-list 'org-capture-templates
                   `("c" "Contacts" entry (file "~/.org-files/contacts.org") "* %(org-contacts-template-name)
 :PROPERTIES:
 :NAME:
 :PHONE:
 :EMAIL: %(org-contacts-template-email)
 :BIRTHDAY:
 :NICKNAME:
 :NOTE:
 :ADDRESS:
 :END:") t)

      ;; Enable habit tracking (and a bunch of other modules)
      (setq org-modules
            (quote (org-bbdb
                    org-bibtex
                    org-crypt
                    org-gnus
                    org-id
                    org-info
                    ;;org-jsinfo
                    org-habit
                    org-inlinetask
                    org-irc
                    org-mew
                    org-mhe
                    org-protocol
                    org-rmail
                    org-vm
                    org-wl
                    org-w3m)))

      (defconst pelm-org-completed-date-regexp
        (concat "\\("
                "CLOSED: \\[%Y-%m-%d"
                "\\|"
                "- State \"\\(DONE\\|CANX\\)\" * from .* \\[%Y-%m-%d"
                "\\|"
                "- State .* ->  *\"\\(DONE\\|CANX\\)\" * \\[%Y-%m-%d"
                "\\)")
        "Matches any completion time stamp.")

      ;; Custom agenda command definitions -- start with a clean state
      (setq org-agenda-custom-commands nil)

      (add-to-list 'org-agenda-custom-commands
                   '("c" . "COLLECT...") t)

      (add-to-list 'org-agenda-custom-commands
                   '("cb" "Collect Box" tags "REFILE"
                     ((org-agenda-overriding-header "Tasks to Refile")
                      (org-tags-match-list-sublevels nil))))

      (add-to-list 'org-agenda-custom-commands
                   `("h" "Habits"
                     ((alltodo ""))
                     ((org-agenda-files (list ,(concat org-directory "/habits.org"))))) t)

      (add-to-list 'org-agenda-custom-commands '("f" . "FOCUS...") t )

      (add-to-list 'org-agenda-custom-commands
                   `("f." "Today"
                     (
                      ;; Events.
                      (agenda ""
                              ((org-agenda-entry-types '(:timestamp :sexp))
                               (org-agenda-overriding-header
                                (concat "CALENDAR Today " (format-time-string "%a %d" (current-time))))
                               (org-agenda-span 'day)))
                      ;; Unscheduled new tasks (waiting to be prioritized and scheduled).
                      (tags-todo "LEVEL=2"
                                 ((org-agenda-overriding-header "COLLECTBOX (Unscheduled)")
                                  (org-agenda-files (list ,(concat org-directory "/refile.org")))))

                      (tags-todo "TODO={INPROGRESS}"
                                 ((org-agenda-overriding-header "INPROGRESS TASKS")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'deadline))))

                      ;; List of all TODO entries with deadline today.
                      (tags-todo "DEADLINE=\"<+0d>\""
                                 ((org-agenda-overriding-header "DUE TODAY")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'notdeadline))
                                  (org-agenda-sorting-strategy '(priority-down))))
                                        ; XXX Timed deadlines NOT shown!!!
                      ;; List of all TODO entries with deadline before today.
                      (tags-todo "DEADLINE<\"<+0d>\""
                                 ((org-agenda-overriding-header "OVERDUE")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'notdeadline))
                                  (org-agenda-sorting-strategy '(priority-down))))
                      (agenda ""
                              ((org-agenda-entry-types '(:scheduled))
                               (org-agenda-overriding-header "SCHEDULED")
                               (org-agenda-skip-function
                                '(org-agenda-skip-entry-if 'todo 'done))
                               (org-agenda-sorting-strategy
                                '(priority-down time-up))
                               (org-agenda-span 'day)
                               (org-agenda-start-on-weekday nil)
                               (org-agenda-time-grid nil)))
                      ;; List of all TODO entries completed today.
                      (todo "TODO|DONE|CANX" ; Includes repeated tasks (back in TODO).
                            ((org-agenda-overriding-header "COMPLETED TODAY")
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if
                                'notregexp
                                (format-time-string pelm-org-completed-date-regexp)))
                             (org-agenda-sorting-strategy '(priority-down)))))
                     ((org-agenda-format-date "")
                      (org-agenda-start-with-clockreport-mode nil))) t)

      (add-to-list 'org-agenda-custom-commands
                   '("fh" "Hotlist"
                     ((tags-todo "DEADLINE<\"<+0d>\""
                                 ((org-agenda-overriding-header "OVERDUE")))
                      (tags-todo "DEADLINE>=\"<+0d>\"+DEADLINE<=\"<+1w>\""
                                 ((org-agenda-overriding-header "DUE IN NEXT 7 DAYS")))
                      (tags-todo "DEADLINE=\"\"+PRIORITY={A}|DEADLINE>\"<+1w>\"+PRIORITY={A}"
                                 ((org-agenda-overriding-header "HIGH PRIORITY")))
                      (tags-todo "DEADLINE=\"\"+FLAGGED|DEADLINE>\"<+1w>\"+FLAGGED"
                                 ((org-agenda-overriding-header "FLAGGED")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-when-regexp-matches))
                                  (org-agenda-skip-regexp "\\[#A\\]"))))
                     ((org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-sorting-strategy '(deadline-up)))) t) ; FIXME sort not OK

      (add-to-list 'org-agenda-custom-commands '("r" . "REVIEW...") t)

      (add-to-list 'org-agenda-custom-commands '("ra" . "All Tasks...") t)

      (add-to-list 'org-agenda-custom-commands
                   '("rad" "All Tasks (grouped by Due Date)"
                     ((tags-todo "DEADLINE<\"<+0d>\""
                                 ((org-agenda-overriding-header "OVERDUE")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'notdeadline))))
                      (tags-todo "DEADLINE=\"<+0d>\""
                                 ((org-agenda-overriding-header "DUE TODAY")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'notdeadline))))
                      (tags-todo "DEADLINE=\"<+1d>\""
                                 ((org-agenda-overriding-header "DUE TOMORROW")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'notdeadline))))
                      (tags-todo "DEADLINE>\"<+1d>\"+DEADLINE<=\"<+7d>\""
                                 ((org-agenda-overriding-header "DUE WITHIN A WEEK")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'notdeadline))))
                      (tags-todo "DEADLINE>\"<+7d>\"+DEADLINE<=\"<+28d>\""
                                 ((org-agenda-overriding-header "DUE WITHIN A MONTH")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'notdeadline))))
                      (tags-todo "DEADLINE>\"<+28d>\""
                                 ((org-agenda-overriding-header "DUE LATER")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'notdeadline))))

                      (tags-todo "TODO={INPROGRESS}"
                                 ((org-agenda-overriding-header "NO DUE DATE / STARTED")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'deadline))))
                      (tags-todo "TODO<>{STRT\\|WAIT\\|SOMEDAY\\|HABIT}"
                                 ((org-agenda-overriding-header "NO DUE DATE / NEXT")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'deadline))))
                      (tags-todo "TODO={WAIT}"
                                 ((org-agenda-overriding-header "NO DUE DATE / WAITING FOR")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'deadline))))
                      (tags-todo "TODO={SOMEDAY}"
                                 ((org-agenda-overriding-header "NO DUE DATE / SOMEDAY")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'deadline)))))
                     ((org-agenda-sorting-strategy '(priority-down))
                      (org-agenda-write-buffer-name "All Tasks (grouped by Due Date)"))
                     "~/org___all-tasks-by-due-date.pdf") t)

      (add-to-list 'org-agenda-custom-commands
                   '("ra1" "All Tasks with a due date"
                     ((alltodo ""))
                     ((org-agenda-overriding-header "All Tasks (sorted by Due Date)")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'notdeadline))
                      (org-agenda-sorting-strategy '(deadline-up)))) t)

      (add-to-list 'org-agenda-custom-commands
                   `("ra2" "All active tasks, by due date"
                     ((agenda ""
                              ((org-agenda-overriding-header "Today")
                               ;; FIXME We don't see "timed" DEADLINE.
                               (org-agenda-skip-function
                                (lambda ()
                                  (let* ((dl (org-entry-get nil "DEADLINE")))
                                    (if (or (not dl)
                                            (equal dl "")
                                            (org-time> dl (org-time-today)))
                                        (progn (outline-next-heading) (point))))))
                               (org-agenda-skip-scheduled-if-deadline-is-shown t)
                               (org-agenda-span 'day)
                               (org-deadline-warning-days 0)))
                      (agenda ""
                              ((org-agenda-entry-types '(:deadline))
                               (org-agenda-overriding-header "Tomorrow")
                               (org-agenda-skip-function
                                '(pelm-org/skip-entry-unless-deadline-in-n-days-or-more 1))
                               (org-deadline-warning-days 1)))
                      (agenda ""
                              ((org-agenda-overriding-header "Next 5 days")
                               (org-agenda-skip-function
                                '(pelm-org/skip-entry-unless-deadline-in-n-days-or-more 2))
                               (org-deadline-warning-days 7)))
                      (agenda ""
                              ((org-agenda-format-date "")
                               (org-agenda-overriding-header "Next 3 weeks")
                               (org-agenda-skip-function
                                '(pelm-org/skip-entry-unless-deadline-in-n-days-or-more 7))
                               (org-deadline-warning-days 28))))
                     ((org-agenda-deadline-faces '((0.0 . default)))
                      (org-agenda-start-with-clockreport-mode nil)
                      (org-agenda-format-date "")
                      (org-agenda-span 'day)
                      (org-agenda-sorting-strategy '(deadline-up))
                      (org-agenda-use-time-grid nil)
                      (org-agenda-write-buffer-name "Reminders"))) t)

      (defun pelm-org/skip-entry-unless-deadline-in-n-days-or-more (n)
        "Skip entries that have no deadline, or that have a deadline earlier than in N days."
        (let* ((dl (org-entry-get nil "DEADLINE")))
          (if (or (not dl)
                  (equal dl "")
                  (org-time< dl (+ (org-time-today) (* n 86400))))
              (progn (outline-next-heading) (point)))))


      (defun pelm-org/skip-entry-unless-overdue-deadline ()
        "Skip entries that have no deadline, or that have a deadline later than or equal to today."
        (let* ((dl (org-entry-get nil "DEADLINE")))
          (if (or (not dl)
                  (equal dl "")
                  (org-time>= dl (org-time-today)))
              (progn (outline-next-heading) (point)))))

      (defun pelm-org/skip-entry-if-past-deadline ()
        "Skip entries that have a deadline earlier than today."
        (let* ((dl (org-entry-get nil "DEADLINE")))
          (if (org-time< dl (org-time-today))
              (progn (outline-next-heading) (point)))))

      (defun pelm-org/skip-entry-if-deadline-in-less-than-n-days-or-schedule-in-less-than-n-days (n1 n2)
        "Skip entries that have a deadline in less than N1 days, or that have a
  scheduled date in less than N2 days, or that have no deadline nor scheduled."
        (let* ((dl (org-entry-get nil "DEADLINE"))
               (sd (org-entry-get nil "SCHEDULED")))
          (if (or (and dl
                       (not (equal dl ""))
                       (org-time< dl (+ (org-time-today) (* n1 86400))))
                  (and sd
                       (not (equal sd ""))
                       (org-time< sd (+ (org-time-today) (* n2 86400))))
                  (and (or (not dl)       ; No deadline.
                           (equal dl ""))
                       (or (not sd)       ; Nor scheduled.
                           (equal sd ""))))
              (progn (outline-next-heading) (point)))))

      (defun pelm-org/skip-entry-if-deadline-or-schedule ()
        "Skip entries that have a deadline or that have a scheduled date."
        (let* ((dl (org-entry-get nil "DEADLINE"))
               (sd (org-entry-get nil "SCHEDULED")))
          (if (or (and dl
                       (not (equal dl "")))
                  (and sd
                       (not (equal sd ""))))
              (progn (outline-next-heading) (point)))))

      (defun pelm-org/skip-entry-if-deadline-in-less-than-n-days-or-schedule-in-less-than-n-days (n1 n2)
        "Skip entries that have a deadline in less than N1 days, or that have a
  scheduled date in less than N2 days, or that have no deadline nor scheduled."
        (let* ((dl (org-entry-get nil "DEADLINE"))
               (sd (org-entry-get nil "SCHEDULED")))
          (if (or (and dl
                       (not (equal dl ""))
                       (org-time< dl (+ (org-time-today) (* n1 86400))))
                  (and sd
                       (not (equal sd ""))
                       (org-time< sd (+ (org-time-today) (* n2 86400))))
                  (and (or (not dl)       ; No deadline.
                           (equal dl ""))
                       (or (not sd)       ; Nor scheduled.
                           (equal sd ""))))
              (progn (outline-next-heading) (point)))))

      (add-to-list 'org-agenda-custom-commands
                   '("ra3" "Agenda for all TODO entries"
                     ((agenda ""
                              ((org-agenda-format-date "")
                               (org-agenda-overriding-header "Past due")
                               (org-agenda-skip-function
                                'pelm-org/skip-entry-unless-overdue-deadline)
                               (org-deadline-warning-days 0)))
                      (agenda ""
                              ((org-agenda-format-date "")
                               (org-agenda-overriding-header "Today/tomorrow")
                               (org-agenda-skip-function
                                'pelm-org/skip-entry-if-past-deadline)
                               (org-agenda-span 2)
                               (org-agenda-use-time-grid t)
                               (org-deadline-warning-days 0)))
                      (agenda ""
                              ((org-agenda-format-date "")
                               (org-agenda-overriding-header "Next 12 days")
                               (org-agenda-skip-function
                                '(pelm-org/skip-entry-unless-deadline-in-n-days-or-more 2))
                               (org-deadline-warning-days 14)))
                      (todo ""
                            ((org-agenda-overriding-header "Later")
                             (org-agenda-skip-function
                              '(pelm-org/skip-entry-if-deadline-in-less-than-n-days-or-schedule-in-less-than-n-days 15 2))
                             (org-agenda-sorting-strategy '(ts-up))))
                      (todo ""
                            ((org-agenda-overriding-header "No due date")
                             (org-agenda-skip-function
                              'pelm-org/skip-entry-if-deadline-or-schedule))))
                     ((org-agenda-start-with-clockreport-mode nil)
                      (org-agenda-prefix-format " %i %?-12t% s")
                      (org-agenda-span 'day)
                      (org-agenda-use-time-grid nil)
                      (org-agenda-sorting-strategy '(deadline-up)) ; FIXME sort does not work in "Past due", well in "Next 12 days".
                      (org-agenda-write-buffer-name "List Review"))
                     "~/org___agenda-all-todo-entries.html") t)

      (add-to-list 'org-agenda-custom-commands
                   '("rap" "All (Unscheduled) Tasks (grouped by Priority)"
                     ((tags-todo "PRIORITY={A}"
                                 ((org-agenda-overriding-header "HIGH")
                                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                      (tags-todo "PRIORITY={B}"
                                 ((org-agenda-overriding-header "MEDIUM")
                                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                      (tags-todo "PRIORITY=\"\""
                                 ((org-agenda-overriding-header "NONE") ; = Medium.
                                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                      (tags-todo "PRIORITY={C}"
                                 ((org-agenda-overriding-header "LOW")
                                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                      (todo "DONE|CANX"
                            ((org-agenda-overriding-header "COMPLETED")
                             (org-agenda-sorting-strategy '(priority-down)))))) t)

      (add-to-list 'org-agenda-custom-commands
                   '("rt" . "Timesheet...") t)

      ;; Show what happened today.
      (add-to-list 'org-agenda-custom-commands
                   '("rtd" "Daily Timesheet"
                     ((agenda ""))
                     ((org-agenda-log-mode-items '(clock closed))
                      (org-agenda-overriding-header "DAILY TIMESHEET")
                      (org-agenda-show-log 'clockcheck)
                      (org-agenda-span 'day)
                      (org-agenda-start-with-clockreport-mode t)
                      (org-agenda-time-grid nil))) t)

      ;; Show what happened this week.
      (add-to-list 'org-agenda-custom-commands
                   '("rtw" "Weekly Timesheet"
                     ((agenda ""))
                     (
                      ;; (org-agenda-format-date "")
                      (org-agenda-overriding-header "WEEKLY TIMESHEET")
                      ;;(org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp))
                      (org-agenda-span 'week)
                      (org-agenda-start-on-weekday 1)
                      (org-agenda-start-with-clockreport-mode t)
                      (org-agenda-time-grid nil))) t)


      (add-to-list 'org-agenda-custom-commands '("rc" . "Calendar...") t)

      (add-to-list 'org-agenda-custom-commands
                   '("rc7" "Events and appointments for 7 days"
                     ((agenda ""))
                     ((org-agenda-entry-types '(:timestamp :sexp))
                      ;; (org-agenda-overriding-header "Calendar for 7 days")
                      ;; (org-agenda-repeating-timestamp-show-all t)
                      (org-agenda-span 'week)
                      (org-agenda-format-date "\n%a %d")
                      ;; (org-agenda-date-weekend ... new face ...)
                      (org-agenda-time-grid nil))) t)

      ;; Calendar view for org-agenda.
      (when (locate-library "calfw-org")
        (autoload 'cfw:open-org-calendar "calfw-org"
          "Open an Org schedule calendar." t)

        (add-to-list 'org-agenda-custom-commands
                     '("rcm" "Calendar for current month"
                       (lambda (&rest ignore)
                         (cfw:open-org-calendar))) t))

  (add-to-list 'org-agenda-custom-commands
               `("rC" "Completed view"
                 (;; List of all TODO entries completed yesterday.
                  (todo "TODO|DONE|CANX" ; includes repeated tasks (back in TODO)
                             ((org-agenda-overriding-header
                               (concat "YESTERDAY   "
                                       (format-time-string "%a %d" (current-time-ndays-ago 1))
                                       ;; #("__________________" 0 12 (face (:foreground "gray")))
                                       ))
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if
                                 'notregexp
                                 (format-time-string leuven-org-completed-date-regexp (current-time-ndays-ago 1))))
                              (org-agenda-sorting-strategy '(priority-down))))
                  ;; List of all TODO entries completed 2 days ago.
                  (todo "TODO|DONE|CANX" ; includes repeated tasks (back in TODO)
                             ((org-agenda-overriding-header
                               (concat "2 DAYS AGO  "
                                       (format-time-string "%a %d" (current-time-ndays-ago 2))))
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if
                                 'notregexp
                                 (format-time-string leuven-org-completed-date-regexp (current-time-ndays-ago 2))))
                              (org-agenda-sorting-strategy '(priority-down))))
                  ;; List of all TODO entries completed 3 days ago.
                  (todo "TODO|DONE|CANX" ; Includes repeated tasks (back in TODO).
                             ((org-agenda-overriding-header
                               (concat "3 DAYS AGO  "
                                       (format-time-string "%a %d" (current-time-ndays-ago 3))))
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if
                                 'notregexp
                                 (format-time-string leuven-org-completed-date-regexp (current-time-ndays-ago 3))))
                              (org-agenda-sorting-strategy '(priority-down)))))
                 ((org-agenda-format-date "")
                  (org-agenda-start-with-clockreport-mode nil))) t)

  (defun current-time-ndays-ago (n)
    "Return the current time minus N days."
    (time-subtract (current-time) (days-to-time n)))

  (add-to-list 'org-agenda-custom-commands
               '("rx" "Completed tasks with no CLOCK lines"
                 ((todo "DONE|CANX"
                             ((org-agenda-overriding-header "Completed tasks with no CLOCK lines")
                              (org-agenda-skip-function
                               '(org-agenda-skip-entry-if
                                 'regexp
                                 (format-time-string "  CLOCK: .*--.* =>  .*")))
                              (org-agenda-sorting-strategy '(priority-down)))))) t)

  (add-to-list 'org-agenda-custom-commands
               '("rr" "Recent items (past 7 days)"
                 ((agenda ""))
                 ((org-agenda-start-day "-7d")
                  (org-agenda-span 7)
                  (org-agenda-repeating-timestamp-show-all nil)
                  (org-agenda-inactive-leader "Inactive:  ")
                  (org-agenda-include-inactive-timestamps t))) t)

  (add-to-list 'org-agenda-custom-commands
               '("rw" "Weekly review"
                 ((tags "CATEGORY={@Collect}&LEVEL=2|TODO={NEW}"
                        ((org-agenda-overriding-header "COLLECTBOX (Unscheduled)")))

                  (agenda ""
                          ((org-agenda-clockreport-mode t)
                           (org-agenda-format-date
                            (concat "\n"
                                    "%Y-%m-%d" " %a "
                                    (make-string (window-width) ?_)))
                           (org-agenda-overriding-header "PAST WEEK")
                           (org-agenda-prefix-format " %?-11t %i %-12:c% s")
                           (org-agenda-show-log 'clockcheck)
                           (org-agenda-span 7)
                           (org-agenda-start-day "-1w") ; recently done
                           (org-deadline-warning-days 0)))

                  (agenda ""
                          ((org-agenda-overriding-header "THIS WEEK")
                           (org-agenda-span 'week)
                           (org-agenda-start-day "+0d")
                           (org-deadline-warning-days 0)))

                  (todo "PROJ"
                        ((org-agenda-overriding-header "PROJECT LIST")))

                  ;; FIXME we should show which tasks (don't) have CLOCK lines: archived vs. deleted.
                  (todo "DONE|PROJDONE"
                        ((org-agenda-overriding-header
                          "Candidates to be archived")))

                  (todo "INPROGRESS"
                        ((org-agenda-overriding-header "IN PROGRESS")
                         (org-agenda-todo-ignore-scheduled nil)))

                  (todo "TODO"
                        ((org-agenda-overriding-header "ACTION LIST")))

                  (todo "WAIT"
                        ((org-agenda-format-date "")
                         (org-agenda-overriding-header "WAITING FOR")
                         (org-agenda-todo-ignore-deadlines 'all) ; Future?
                         (org-agenda-todo-ignore-scheduled t)))

                  ;; Same reasoning as for WAIT.
                  (todo "SOMEDAY"
                        ((org-agenda-format-date "")
                         (org-agenda-overriding-header "SOMEDAY")
                         (org-agenda-todo-ignore-deadlines 'all)
                         (org-agenda-todo-ignore-scheduled t)))
                  )) t)

  (add-to-list 'org-agenda-custom-commands
               '("rN" "Next"
                 ((tags-todo "TODO<>{SOMEDAY}"))
                 ((org-agenda-overriding-header "List of all TODO entries with no due date (no SOMEDAY)")
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                  (org-agenda-sorting-strategy '(priority-down)))) t)

  (add-to-list 'org-agenda-custom-commands
               '("rW" "Waiting for"
                 ((tags-todo "TODO={WAIT}"))
                 ((org-agenda-overriding-header "Waiting for")
                  (org-agenda-sorting-strategy '(deadline-up)))) t) ; FIXME does not work.

  (add-to-list 'org-agenda-custom-commands
               '("rP" "Projects"
                 ((tags-todo "project-DONE-CANX"))
                 ((org-agenda-overriding-header "Projects (High Level)")
                  (org-agenda-sorting-strategy nil))) t)

  (add-to-list 'org-agenda-custom-commands
               '("+" . "MORE...") t)

  ;; Checking tasks that are assigned to me.
  (add-to-list 'org-agenda-custom-commands
               `("+a" "Assigned to me"
                 ((tags ,(concat "Assignee={" user-login-name "\\|"
                                 user-mail-address "}")))
                 ((org-agenda-overriding-header "ASSIGNED TO ME"))) t)

  (add-to-list 'org-agenda-custom-commands
               '("E" . "Exported agenda files...") t)
  ;; Exporting agenda views.
  (add-to-list 'org-agenda-custom-commands
               '("Ea"
                 ((agenda ""))
                 (;; (org-tag-faces nil)
                  (ps-landscape-mode t)
                  (ps-number-of-columns 1))
                 ("~/org-agenda.html" "~/org-agenda.pdf")) t)

  (add-to-list 'org-agenda-custom-commands
               '("Ep" "Call list"
                 ((tags-todo "phone"))
                 ((org-agenda-prefix-format " %-20:c [ ] " )
                  (org-agenda-remove-tags t)
                  ;; (org-agenda-with-colors nil)
                  (org-agenda-write-buffer-name
                   "Phone calls that you need to make")
                  (ps-landscape-mode t)
                  (ps-number-of-columns 1))
                 ("~/org___calls.pdf")) t)

  (add-to-list 'org-agenda-custom-commands
               '("A" . "ARCHIVE...") t)

  (add-to-list 'org-agenda-custom-commands
               '("Aa" "Archive"
                 ((tags-todo "ARCHIVE"))
                 ((org-agenda-todo-ignore-scheduled 'future)
                  (org-agenda-sorting-strategy '(deadline-down)))) t)

  (add-to-list 'org-agenda-custom-commands
               '("R" . "REFERENCE...") t)

  (add-to-list 'org-agenda-custom-commands
               '("Rs" "Like s, but with extra files"
                 ((search ""))
                 ((org-agenda-text-search-extra-files
                   ;; FIXME Add `agenda-archives'
                   leuven-org-search-extra-files))) t)

  (add-to-list 'org-agenda-custom-commands
               '("RS" "Like s, but only TODO entries"
                 ((search ""))
                 ((org-agenda-text-search-extra-files
                   ;; FIXME Add `agenda-archives'
                   leuven-org-search-extra-files))) t)

  (add-to-list 'org-agenda-custom-commands
               '("Rn" "Organize thoughts to refile"
                 ((tags "REFILE|CAPTURE"))
                 ((org-agenda-overriding-header "Refile stuff"))) t)



      ;; TODO: maybe need enable it back.
      ;;(autoload 'appt-check "appt")
      
      ;; Config support
      (defun pelm-org/mark-next-parent-tasks-todo ()
        "Visit each parent task and change INPROGRESS states to TODO"
        (let ((mystate (or (and (fboundp 'state)
                                org-state)
                           (nth 2 (org-heading-components)))))
          (when (equal mystate "INPROGRESS")
            (save-excursion
              (while (org-up-heading-safe)
                (when (member (nth 2 (org-heading-components)) (list "INPROGRESS"))
                  (org-todo "TODO")))))))


      (defun pelm-org/narrow-up-one-org-level ()
        (widen)
        (save-excursion
          (outline-up-heading 1 'invisible-ok)
          (pelm-org/narrow-to-org-subtree)))

      (defun pelm-org/narrow-up-one-level ()
        (interactive)
        (if (equal major-mode 'org-agenda-mode)
            (org-with-point-at (org-get-at-bol 'org-hd-marker)
              (pelm-org/narrow-up-one-org-level))
          (pelm-org/narrow-up-one-org-level)))

      (defun pelm-org/narrow-to-org-subtree ()
        (widen)
        (org-narrow-to-subtree)
        (save-restriction
          (org-agenda-set-restriction-lock)))

      (defun pelm-org/narrow-to-subtree ()
        (interactive)
        (if (equal major-mode 'org-agenda-mode)
            (org-with-point-at (org-get-at-bol 'org-hd-marker)
              (pelm-org/narrow-to-org-subtree))
          (pelm-org/narrow-to-org-subtree)))


      (defun pelm-org/widen ()
        (interactive)
        (widen)
        (org-agenda-remove-restriction-lock))

      (defun pelm-org/org-todo (arg)
        (interactive "p")
        (if (equal arg 4)
            (save-restriction
              (widen)
              (org-narrow-to-org-subtree)
              (org-show-todo-tree nil))
          (widen)
          (org-narrow-to-org-subtree)
          (org-show-todo-tree nil)))

      (setq org-agenda-cmp-user-defined 'pelm-org/agenda-sort)

      (defun pelm-org/agenda-sort (a b)
        "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
        (let (result num-a num-b)
          (cond
           ;; time specific items are already sorted first by org-agenda-sorting-strategy

           ;; non-deadline and non-scheduled items next
           ((pelm-org/agenda-sort-test 'pelm-org/is-not-scheduled-or-deadline a b))

           ;; deadlines for today next
           ((pelm-org/agenda-sort-test 'pelm-org/is-due-deadline a b))

           ;; late deadlines next
           ((pelm-org/agenda-sort-test-num 'pelm-org/is-late-deadline '< a b))

           ;; scheduled items for today next
           ((pelm-org/agenda-sort-test 'pelm-org/is-scheduled-today a b))

           ;; late scheduled items next
           ((pelm-org/agenda-sort-test-num 'pelm-org/is-scheduled-late '> a b))

           ;; pending deadlines last
           ((pelm-org/agenda-sort-test-num 'pelm-org/is-pending-deadline '< a b))

           ;; finally default to unsorted
           (t (setq result nil)))
          result))

      (defmacro pelm-org/agenda-sort-test (fn a b)
        "Test for agenda sort"
        `(cond
                                        ; if both match leave them unsorted
          ((and (apply ,fn (list ,a))
                (apply ,fn (list ,b)))
           (setq result nil))
                                        ; if a matches put a first
          ((apply ,fn (list ,a))
           (setq result -1))
                                        ; otherwise if b matches put b first
          ((apply ,fn (list ,b))
           (setq result 1))
                                        ; if none match leave them unsorted
          (t nil)))

      (defmacro pelm-org/agenda-sort-test-num (fn compfn a b)
        `(cond
          ((apply ,fn (list ,a))
           (setq num-a (string-to-number (match-string 1 ,a)))
           (if (apply ,fn (list ,b))
               (progn
                 (setq num-b (string-to-number (match-string 1 ,b)))
                 (setq result (if (apply ,compfn (list num-a num-b))
                                  -1
                                1)))
             (setq result -1)))
          ((apply ,fn (list ,b))
           (setq result 1))
          (t nil)))

      (defun pelm-org/is-not-scheduled-or-deadline (date-str)
        (and (not (pelm-org/is-deadline date-str))
             (not (pelm-org/is-scheduled date-str))))

      (defun pelm-org/is-due-deadline (date-str)
        (string-match "Deadline:" date-str))


      (defun pelm-org/is-late-deadline (date-str)
        (string-match "In *\\(-.*\\)d\.:" date-str))

      (defun pelm-org/is-pending-deadline (date-str)
        (string-match "In \\([^-]*\\)d\.:" date-str))

      (defun pelm-org/is-deadline (date-str)
        (or (pelm-org/is-due-deadline date-str)
            (pelm-org/is-late-deadline date-str)
            (pelm-org/is-pending-deadline date-str)))

      (defun pelm-org/is-scheduled (date-str)
        (or (pelm-org/is-scheduled-today date-str)
            (pelm-org/is-scheduled-late date-str)))

      (defun pelm-org/is-scheduled-today (date-str)
        (string-match "Scheduled:" date-str))

      (defun pelm-org/is-scheduled-late (date-str)
        (string-match "Sched\.\\(.*\\)x:" date-str))

      ;; Erase all reminders and rebuilt reminders for today from the agenda
      (defun pelm-org/org-agenda-to-appt ()
        (interactive)
        (setq appt-time-msg-list nil)
        (org-agenda-to-appt))

      (defun pelm-org/display-inline-images ()
        (condition-case nil
            (org-display-inline-images)
          (error nil)))

      (defun pelm-org/skip-non-archivable-tasks ()
        "Skip trees that are not available for archiving"
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            ;; Consider only tasks with done todo headings as archivable candidates
            (if (member (org-get-todo-state) org-done-keywords)
                (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                       (daynr (string-to-int (format-time-string "%d" (current-time))))
                       (a-month-ago (* 60 60 24 (+ daynr 1)))
                       (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                       (this-month (format-time-string "%Y-%m-" (current-time)))
                       (subtree-is-current (save-excursion
                                             (forward-line 1)
                                             (and (< (point) subtree-end)
                                                  (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                  (if subtree-is-current
                      next-headline ; Has a date in this month or last month, skip it
                    nil))  ; available to archive
              (or next-headline (point-max))))))


      (defun pelm-org/is-project-p ()
        "Any task with a todo keyword subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task has-subtask))))

      (defun pelm-org/is-project-subtree-p ()
        "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
        (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                    (point))))
          (save-excursion
            (pelm-org/find-project-task)
            (if (equal (point) task)
                nil
              t))))

      (defun pelm-org/is-task-p ()
        "Any task with a todo keyword and no subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task (not has-subtask)))))

      (defun pelm-org/is-subproject-p ()
        "Any task which is a subtask of another project"
        (let ((is-subproject)
              (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
          (save-excursion
            (while (and (not is-subproject) (org-up-heading-safe))
              (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                (setq is-subproject t))))
          (and is-a-task is-subproject)))

      (defun pelm-org/list-sublevels-for-projects-indented ()
        "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
        (if (marker-buffer org-agenda-restrict-begin)
            (setq org-tags-match-list-sublevels 'indented)
          (setq org-tags-match-list-sublevels nil))
        nil)

      (defun pelm-org/list-sublevels-for-projects ()
        "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
        (if (marker-buffer org-agenda-restrict-begin)
            (setq org-tags-match-list-sublevels t)
          (setq org-tags-match-list-sublevels nil))
        nil)

      (defun pelm-org/remove-empty-drawer-on-clock-out ()
        "Remove empty LOGBOOK drawers on clock out."
        (interactive)
        (save-excursion
          (beginning-of-line 0)
          ;;    (org-remove-empty-drawer-at "LOGBOOK" (point))))
          (org-remove-empty-drawer-at (point))))

      (defun pelm-org/verify-refile-target ()
        "Exclude todo keywords with a done state from refile targets."
        (not (member (nth 2 (org-heading-components)) org-done-keywords)))

      (defun pelm-org/org-auto-exclude-function (tag)
        "Automatic task exclusion in the agenda with / RET"
        (and (cond
              (
               (string= tag "hold") t)
              )
             (concat "-" tag)))

      (setq org-agenda-auto-exclude-function 'pelm-org/org-auto-exclude-function)

      (defun pelm-org/set-truncate-lines ()
        "Toggle value of truncate-lines and refresh window display."
        (interactive)
        (setq truncate-lines (not truncate-lines))
        ;; now refresh window display (an idiom from simple.el):
        (save-excursion
          (set-window-start (selected-window)
                            (window-start (selected-window)))))

      (defun pelm-org/org-auto-exclude-function (tag)
        "Automatic task exclusion in the agenda with / RET"
        (and (cond
              ((string= tag "hold")         t)
              )
             (concat "-" tag)))

      (defun pelm-org/clock-in-to-next (kw)
        "Switch a task from TODO to INPROGRESS when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from INPROGRESS back to TODO"
        (when (not (and (boundp 'org-capture-mode) org-capture-mode))
          (cond
           ((and (member (org-get-todo-state) (list "TODO"))
                 (pelm-org/is-task-p))
            "INPROGRESS")
           ((and (member (org-get-todo-state) (list "INPROGRESS"))
                 (pelm-org/is-project-p))
            "TODO"))))

      (defun pelm-org/find-project-task ()
        "Move point to the parent (project) task if any"
        (save-restriction
          (widen)
          (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
            (while (org-up-heading-safe)
              (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                (setq parent-task (point))))
            (goto-char parent-task)
            parent-task)))

      (defun pelm-org/punch-in (arg)
        "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
        (interactive "p")
        (setq pelm-org/keep-clock-running t)
        (if (equal major-mode 'org-agenda-mode)
            ;;
            ;; We're in the agenda
            ;;
            (let* ((marker (org-get-at-bol 'org-hd-marker))
                   (tags (org-with-point-at marker (org-get-tags-at))))
              (if (and (eq arg 4) tags)
                  (org-agenda-clock-in '(16))
                (pelm-org/clock-in-organization-task-as-default)))
          ;;
          ;; We are not in the agenda
          ;;
          (save-restriction
            (widen)
                                        ; Find the tags on the current task
            (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
                (org-clock-in '(16))
              (pelm-org/clock-in-organization-task-as-default)))))

      (defun pelm-org/punch-out ()
        (interactive)
        (setq pelm-org/keep-clock-running nil)
        (when (org-clock-is-active)
          (org-clock-out))
        (org-agenda-remove-restriction-lock))

      (defun pelm-org/clock-in-default-task ()
        (save-excursion
          (org-with-point-at org-clock-default-task
            (org-clock-in))))

      (defun pelm-org/clock-in-parent-task ()
        "Move point to the parent (project) task if any and clock in"
        (let ((parent-task))
          (save-excursion
            (save-restriction
              (widen)
              (while (and (not parent-task) (org-up-heading-safe))
                (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                  (setq parent-task (point))))
              (if parent-task
                  (org-with-point-at parent-task
                    (org-clock-in))
                (when pelm-org/keep-clock-running
                  (pelm-org/clock-in-default-task)))))))


      (defun pelm-clock-in-task-by-id (task-id)
        (interactive)
        (org-with-point-at (org-id-find task-id 'marker)
          (org-clock-in '(16))))

      (defun pelm-org/clock-out-maybe ()
        (when (and pelm-org/keep-clock-running
                   (not org-clock-clocking-in)
                   (marker-buffer org-clock-default-task)
                   (not org-clock-resolving-clocks-due-to-idleness))
          (pelm-org/clock-in-parent-task)))

      (defun pelm-org/clock-in-last-task (arg)
        "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
        (interactive "p")
        (let ((clock-in-to-task
               (cond
                ((eq arg 4) org-clock-default-task)
                ((and (org-clock-is-active)
                      (equal org-clock-default-task (cadr org-clock-history)))
                 (caddr org-clock-history))
                ((org-clock-is-active) (cadr org-clock-history))
                ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
                (t (car org-clock-history)))))
          (org-with-point-at clock-in-to-task
            (org-clock-in nil))))


      (defun pelm-org/is-project-p ()
        "Any task with a todo keyword subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task has-subtask))))

      (defun pelm-org/is-project-subtree-p ()
        "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
        (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                    (point))))
          (save-excursion
            (pelm-org/find-project-task)
            (if (equal (point) task)
                nil
              t))))

      (defun pelm-org/is-task-p ()
        "Any task with a todo keyword and no subtask"
        (save-restriction
          (widen)
          (let ((has-subtask)
                (subtree-end (save-excursion (org-end-of-subtree t)))
                (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
            (save-excursion
              (forward-line 1)
              (while (and (not has-subtask)
                          (< (point) subtree-end)
                          (re-search-forward "^\*+ " subtree-end t))
                (when (member (org-get-todo-state) org-todo-keywords-1)
                  (setq has-subtask t))))
            (and is-a-task (not has-subtask)))))

      (defun pelm-org/is-subproject-p ()
        "Any task which is a subtask of another project"
        (let ((is-subproject)
              (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
          (save-excursion
            (while (and (not is-subproject) (org-up-heading-safe))
              (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
                (setq is-subproject t))))
          (and is-a-task is-subproject)))

      (defun pelm-org/list-sublevels-for-projects-indented ()
        "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
        (if (marker-buffer org-agenda-restrict-begin)
            (setq org-tags-match-list-sublevels 'indented)
          (setq org-tags-match-list-sublevels nil))
        nil)

      (defun pelm-org/list-sublevels-for-projects ()
        "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
        (if (marker-buffer org-agenda-restrict-begin)
            (setq org-tags-match-list-sublevels t)
          (setq org-tags-match-list-sublevels nil))
        nil)

      (setq org-agenda-clock-consistency-checks
            (quote (:max-duration "10:30"
                                  :min-duration 0
                                  :max-gap 0
                                  :gap-ok-around ("10:30"))))

      ;; shows 1 minute clocking gaps
      (setq org-time-stamp-rounding-minutes (quote (1 1)))

      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "W" 'pelm-org/widen))
                'append)

      (add-hook 'org-agenda-mode-hook
                '(lambda () (hl-line-mode t))
                'append)


      (add-hook 'org-babel-after-execute-hook 'pelm-org/display-inline-images 'append)

      (add-hook 'org-after-todo-state-change-hook 'pelm-org/mark-next-parent-tasks-todo 'append)
      (add-hook 'org-clock-in-hook 'pelm-org/mark-next-parent-tasks-todo 'append)

      (add-hook 'org-clock-out-hook 'pelm-org/clock-out-maybe 'append)

      (add-hook 'org-clock-out-hook 'pelm-org/remove-empty-drawer-on-clock-out 'append)

      (add-hook 'org-finalize-agenda-hook 'pelm-org/org-agenda-to-appt 'append)
      (setq org-src-fontify-natively t
            org-confirm-babel-evaluate nil)
      ;; disable q key
      (add-hook 'org-agenda-mode-hook
                (lambda ()
                  (define-key org-agenda-mode-map "q" 'bury-buffer))
                'append)

      (org-babel-do-load-languages
       (quote org-babel-load-languages)
       (quote ((emacs-lisp . t)
               (dot . t)
               (ditaa . t)
               ;;(R . t)
               ;;(ledger . t)
               ;;(haskell . t)
               ;;(python . t)
               ;;(ruby . t)
               ;;(scala . t)
               (clojure . t)
               (sh . t)
               (js . t)
               ;; (java . t)
               (org . t)
               ;; (http . t)
               ;;(ipython . t)
               ;;(kotlin . t)
               (plantuml . t)
               ;;(latex . t)
               )))


      )
  ))


(defun pelm-org/post-init-gnuplot ()
  (use-package gnuplot
    :defer t
    :init
    (progn
      (setq gnuplot-image-format "png")
      (setq gnuplot-inline-image-mode 'dedicated)
      (add-hook 'gnuplot-mode-hook 'page-break-lines-mode)

      (defadvice org-plot/gnuplot (around display-buffer activate)
        (ignore-errors ad-do-it)
        (-when-let (buf (get-buffer gnuplot-image-buffer-name))
          (display-buffer buf))))))


(defun pelm-org/init-ox-reveal()
  (use-package ox-reveal
    :mode ("\\.org$" . org-mode)
    :defer t
    :config
    (setq org-reveal-root "file:///Users/eggcaker/src/apps/reveal.js/")))


