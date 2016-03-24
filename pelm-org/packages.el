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

(defconst pelm-org-packages '(org org-ac gnuplot ox-reveal ) )


(defun pelm-org/init-org-ac()
  (use-package org-ac
    :defer t
    :config
    (progn
      (org-ac/config-default))))

(defun pelm-org/post-init-org ()
  (use-package org
    :mode ("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)
    :defer t
    :commands (org-mode
               org-edit-src-exit
               org-refile
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
      (evil-leader/set-key "oj"
        (lambda ()
          (interactive)
          ;(setq current-prefix-arg '(4))
          (org-refile '(4))))
      (global-set-key (kbd "<f6>") (kbd "C-c a ."))
      (global-set-key (kbd "<f13>") (kbd "C-c a g p"))

      ;; set org agenda global
      (spacemacs/declare-prefix "o" "org")
      (spacemacs/set-leader-keys
        "oo" 'org-agenda
        "ob" 'org-iswitchb
        "og" 'org-clock-goto
        "oc" 'org-capture
        "oC" 'org-gcal-sync
        "os" 'org-search-view
        "oI" 'pelm-org/punch-in
        "oO" 'pelm-org/punch-out))
    :config
    (progn

      ;; start of test code
      ;;TODO:  easy clock-in code should be play with
      ;;       (defmacro pelm-org/org-with-current-task (&rest body)
      ;;         "Execute BODY with the point at the subtree of the current task."
      ;;         `(if (derived-mode-p 'org-agenda-mode)
      ;;              (save-window-excursion
      ;;                (org-agenda-switch-to)
      ;;                ,@body)
      ;;            ,@body))

      ;;       (defun pelm-org/org-clock-in-and-track ()
      ;;         "Start the clock running. Clock into Quantified Awesome."
      ;;         (interactive)
      ;;         (pelm-org/org-with-current-task
      ;;          (org-clock-in)
      ;;          (when (org-entry-get (point) "AUTO")
      ;;            (org-open-link-from-string (org-entry-get (point) "AUTO")))))

      ;;       (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)

      ;;       (defun pelm-org/org-quick-clock-in-task (location jump)
      ;;         "Track and clock in on the specified task.
      ;; If JUMP is non-nil or the function is called with the prefix argument, jump to that location afterwards."
      ;;         (interactive)
      ;;         (when location
      ;;           (if jump
      ;;               (progn (org-refile 4 nil location) (pelm-org/org-clock-in-and-track))
      ;;             (save-window-excursion
      ;;               (org-refile 4 nil location)
      ;;               (pelm-org/org-clock-in-and-track)))))
      ;;       (bind-key "C-c q" 'pelm-org/org-quick-clock-in-task)


      ;; end of test code

      ;; (defvar pelm-org/organization-task-id "87043F9F-107D-4AF6-AAC1-D5C31455463A")

      (defun pelm-org/place-agenda-tags ()
        "Put the agenda tags by the right border of the agenda window."
        (setq org-agenda-tags-column (- 2 (window-width)))
        (org-agenda-align-tags))

      (add-hook 'org-finalize-agenda-hook 'pelm-org/place-agenda-tags)

      (defun pelm-org/org-clock-out-if-waiting ()
        "Clock out when the task is marked WAITING."
        (when (and (string= org-state "WAITING")
                   (equal (marker-buffer org-clock-marker) (current-buffer))
                   (< (point) org-clock-marker)
                   (> (save-excursion (outline-next-heading) (point))
                      org-clock-marker)
                   (not (string= org-last-state org-state)))
          (org-clock-out)))
      (add-hook 'org-after-todo-state-change-hook 'pelm-org/org-clock-out-if-waiting)


      ;; just for fun
      (setq org-agenda-category-icon-alist
            '(("[Ee]macs" "~/.spacemacs.d/pelm-org/vendor/icons/org/emacs.png" nil nil :ascent center)
              ("Pacer" "~/.spacemacs.d/pelm-org/vendor/icons/org/pacer.png" nil nil :ascent center)
              ("Refile" "~/.spacemacs.d/pelm-org/vendor/icons/org/inbox.png" nil nil :ascent center)
              ("Habit[s]" "~/.spacemacs.d/pelm-org/vendor/icons/org/habit.png" nil nil :ascent center)
              ("Business" "~/.spacemacs.d/pelm-org/vendor/icons/org/business.png" nil nil :ascent center)
              ("\\(google\\|Calendar\\)" "~/.spacemacs.d/pelm-org/vendor/icons/org/calendar.png" nil nil :ascent center)
              ("Blog" "~/.spacemacs.d/pelm-org/vendor/icons/org/blog.png" nil nil :ascent center)
              ("Geek" "~/.spacemacs.d/pelm-org/vendor/icons/org/geek.png" nil nil :ascent center)
              ("\\(Personal\\|People\\)" "~/.spacemacs.d/pelm-org/vendor/icons/org/personal.svg" nil nil :ascent center)
              ("Learn" "~/.spacemacs.d/pelm-org/vendor/icons/org/learn.png" nil nil :ascent center)
              ("Org" "~/.spacemacs.d/pelm-org/vendor/icons/org/org.png" nil nil :ascent center)
              ("Reading" "~/.spacemacs.d/pelm-org/vendor/icons/org/book.png" nil nil :ascent center)
              ("\\(Holidays\\|Vacation\\)" "~/.spacemacs.d/pelm-org/vendor/icons/org/holidays.png" nil nil :ascent center)
              (".*" '(space . (:width (20))))))

      (setq org-html-checkbox-type 'unicode)
      (setq org-html-checkbox-types
            '((unicode (on . "<span class=\"task-done\">&#x2611;</span>")
                       (off . "<span class=\"task-todo\">&#x2610;</span>")
                       (trans . "<span class=\"task-in-progress\">[-]</span>"))))
      (defvar pelm-org/org-agenda-contexts
        '(
          ;;(tags-todo "+@phone")
          (tags-todo "+@work-HABIT")
          (tags-todo "+@emacs")
          ;;(tags-todo "+@coding")
          (tags-todo "+@reading-HABIT")
          (tags-todo "+@learn")
          ;;(tags-todo "+@computer")
          (tags-todo "+@home"))
        "Usual list of contexts.")


      (setq org-tag-alist '(("@work" . ?w)
                            ("@home" . ?h)
                            ("@emacs" . ?e)
                            ;;("@writing" . ?b)
                            ;;("@coding" . ?c)
                            ;;("@phone" . ?p)
                            ("@reading" . ?r)
                            ;;("@computer" . ?l)
                            ))

      (defun pelm-org/org-agenda-skip-scheduled ()
        (org-agenda-skip-entry-if 'scheduled 'deadline 'regexp "\n]+>"))

      (defmacro measure-time (message &rest body)
        "Measure the time it takes to evaluate BODY."
        `(let ((start (current-time)))
           ,@body
           (message "__%s (in %.02f s)___________________________"
                    ,message (float-time (time-since start)))))


      (defun pelm-org/org-mode-ask-effort ()
        "Ask for an effort estimate when clocking in."
        (unless (org-entry-get (point) "Effort")
          (let ((effort
                 (completing-read
                  "Effort: "
                  (org-entry-get-multivalued-property (point) "Effort"))))
            (unless (equal effort "")
              (org-set-property "Effort" effort)))))

      (add-hook 'org-clock-in-prepare-hook 'pelm-org/org-mode-ask-effort)

      ;; Enable filtering by effort eastimate, that way, it's easy to see
      ;; short tasks that I can finish
      (add-to-list 'org-global-properties
                   '("Effort_ALL" . "0:05 0:15 0:30 1:00 2:00 3:00 4:00" ))

      (defvar pelm-org-mobile-sync-timer nil)

      (defvar pelm-org-mobile-sync-secs (* 60 2))

      (defun pelm-org-mobile-sync-pull-and-push ()
       ;; (org-gcal-sync)
        (org-mobile-pull)
        (org-mobile-push)
        (when (fboundp 'sauron-add-event)
          (sauron-add-event 'my 3 "Called org-mobile-pull and org-mobile-push")))

      (defun pelm-org-mobile-sync ()
         (interactive)
         (org-mobile-pull)
         (org-mobile-push))

      (spacemacs/set-leader-keys
        "op" 'pelm-org-mobile-sync)

      (defun pelm-org-mobile-sync-start ()
        "Start automated `org-mobile-push'"
        (interactive)
        (setq pelm-org-mobile-sync-timer
              (run-with-idle-timer pelm-org-mobile-sync-secs t
                                   'pelm-org-mobile-sync-pull-and-push)))

      (defun pelm-org-mobile-sync-stop ()
        "Stop automated `org-mobile-push'"
        (interactive)
        (cancel-timer pelm-org-mobile-sync-timer))

      (pelm-org-mobile-sync-start)


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

      ;; Remove redundant tags of headlines (from David Maus).
      (defun pelm-org-remove-redundant-tags ()
        "Remove redundant tags of headlines in current buffer.
  A tag is considered redundant if it is local to a headline and inherited by
  a parent headline."
        (interactive)
        (when (derived-mode-p 'org-mode)
          (save-excursion
            (org-map-entries
             (lambda ()
               (let ((alltags (split-string
                               (or (org-entry-get (point) "ALLTAGS") "")
                               ":"))
                     local inherited tag)
                 (dolist (tag alltags)
                   (if (get-text-property 0 'inherited tag)
                       (push tag inherited)
                     (push tag local)))
                 (dolist (tag local)
                   (when (member tag inherited)
                     (org-toggle-tag tag 'off)))))
             t nil))))

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
              (pelm-org-remove-redundant-tags))
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
       org-agenda-sorting-strategy
             '((agenda time-up priority-down tag-up category-keep effort-up)
               ;; (todo user-defined-up todo-state-up priority-down effort-up)
               (todo todo-state-up priority-down effort-up)
               (tags user-defined-up)
               (search category-keep))

       org-directory "~/.org-files"
       org-mobile-directory "~/Dropbox/MobileOrg/"
       org-mobile-inbox-for-pull "~/.org-files/mobileorg.org"
       org-mobile-agendas '("a")
       org-mobile-files '(
                          "~/.org-files/books.org"
                          "~/.org-files/habits.org"
                          )
       org-export-backends '(ascii html md rss reveal icalendar)
       org-show-entry-below (quote ((default)))
       org-agenda-start-on-weekday 7
       org-startup-indented t
       org-goto-interface 'outline-path-completion
       org-goto-max-level 10
       org-log-into-drawer "LOGBOOK"
       org-clock-into-drawer t
       org-todo-repeat-to-state "TODO"
       org-clock-persist-file (concat spacemacs-cache-directory "org-clock-save.el")
       org-startup-with-inline-images t
       org-src-fontify-natively t
       org-ellipsis "⤵"
       org-ditaa-jar-path "~/.spacemacs.d/pelm-org/vendor/ditaa.jar"
       org-plantuml-jar-path "~/.spacemacs.d/pelm-org/vendor/plantuml.jar"
       org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))

       org-alphabetical-lists t
       org-agenda-span 'day
       org-src-fontify-natively t
       org-use-fast-todo-selection t
       org-refile-use-cache nil
       org-treat-S-cursor-todo-selection-as-state-change nil
       org-enforce-todo-dependencies t
       org-track-ordered-property-with-tag t
       org-agenda-dim-blocked-tasks t
       org-habit-preceding-days 7
       org-habit-graph-column 110
       org-tags-column -110
       org-habit-following-days 1
       org-habit-show-habits-only-for-today t
       org-habit-show-all-today t
       org-refile-use-outline-path nil
       org-outline-path-complete-in-steps nil
       org-refile-allow-creating-parent-nodes t
       org-completion-use-ido t
       ido-everywhere t
       ido-max-directory-size 100000
       org-agenda-todo-ignore-with-date nil
       org-agenda-todo-ignore-deadlines nil
       org-agenda-todo-ignore-scheduled nil
       org-agenda-todo-ignore-timestamp nil
       org-agenda-skip-deadline-if-done t
       org-deadline-warning-days 7
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
       org-clock-idle-time nil
       org-log-done 'time
       org-clock-continuously nil
       org-clock-persist t
       org-clock-in-switch-to-state "STARTED"
       org-clock-in-resume nil
       org-show-notification-handler 'message

       org-list-demote-modify-bullet (quote (("+" . "-")
                                             ("*" . "-")
                                             ("1." . "-")
                                             ("1)" . "-")))
       org-refile-targets
       '(
         (nil :maxlevel . 9) ;; only the current file
         (org-agenda-files :maxlevel . 9))

       org-agenda-time-grid '((daily today require-timed)
                              "----------------"
                              (800 1000 1200 1400 1600 1800))
       org-blank-before-new-entry nil
       org-columns-default-format "%14SCHEDULED %Effort{:} %1PRIORITY %TODO %50ITEM %TAGS"
       )

      ;; List of TODO entry keyword sequences (+ fast access keys and specifiers
      ;; for state change logging).
      (setq org-todo-keywords
            '(
              (sequence ;;"NEW(n!)"        ; Proposal, idea (under review), to be prioritized.
                        "TODO(t!)"       ; Open, not yet started
                        "STARTED(s)"     ; In progress, working on
                        "WAIT(w@/!)"     ; On hold , to be discussed, feedback
                        "SOMEDAY(.)"    ; Someday, maybe perhaps.
                        "|"
                        "DONE(d!/!)"     ; Completed, closed fixed, verified
                        "CANCELLED(x!)")      ; Wontfix, rejected, ignored, cancelled

              (sequence "PLAN(p)" "LEARN(l)" "|" "COMPLETE(x)"))

            org-todo-keyword-faces (quote
                                    (("TODO" :foreground "red" :weight bold)
                                     ("NEXT" :foreground "blue" :weight bold)
                                     ("STARTED" :foreground "yellow" :weight bold)
                                     ("DONE" :foreground "forest green" :weight bold)
                                     ("WAITING" :foreground "orange" :weight bold)
                                     ("HOLD" :foreground "magenta" :weight bold)
                                     ("CANCELLED" :foreground "forest green" :weight bold)))

            org-todo-state-tags-triggers
            (quote (("CANCELLED" ("CANCELLED" . t))
                    ("WAITING" ("WAITING" . t))
                    ("HOLD" ("WAITING" . t) ("HOLD" . t))
                    (done ("WAITING") ("HOLD"))
                    ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("STARTED" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))
            )))

      ;; Projects are headings with the :project: tag, so we
      ;; generally don't want that tag inherited, except when we
      ;; display unscheduled tasks that don't belong to any projects.

      (setq org-tags-exclude-from-inheritance '("project"))
      (setq org-stuck-projects
            '("+PROJECT-MAYBE-DONE"
              ("TODO")
              nil
              "\\<IGNORE\\>"))

      (defmacro pelm-org/org-with-current-task (&rest body)
        "Execute BODY with the point at the subtree of the current task."
        `(if (derived-mode-p 'org-agenda-mode)
             (save-window-excursion
               (org-agenda-switch-to)
               ,@body)
           ,@body))

      (defun pelm-org/org-agenda-for-subtree ()
        (interactive)
        (when (derived-mode-p 'org-agenda-mode) (org-agenda-switch-to))
        (pelm-org/org-with-current-task
         (let ((org-agenda-view-columns-initially t))
           (org-agenda nil "t" 'subtree))))

      (add-to-list 'org-speed-commands-user '("T" pelm-org/org-agenda-for-subtree))

      (add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
      (add-to-list 'org-speed-commands-user '("W" widen))

      ;; Create an agenda commands that displays a list of tasks
      ;; by context.
      (defvar pelm-org/org-agenda-limit-items nil
        "Number of items to show in agenda to-do view; nil if unlimited.")

      (defadvice org-agenda-finalize-entries (around sacha activate)
        (if pelm-org/org-agenda-limit-items
            (progn
              (setq list (mapcar 'org-agenda-highlight-todo list))
              (setq ad-return-value
                    (subseq list 0 pelm-org/org-agenda-limit-items))
              (when org-agenda-before-sorting-filter-function
                (setq list (delq nil (mapcar org-agenda-before-sorting-filter-function list))))
              (setq ad-return-value
                    (mapconcat 'identity
                               (delq nil
                                     (subseq
                                      (sort list 'org-entries-lessp)
                                      0
                                      pelm-org/org-agenda-limit-items))
                               "\n")))
          ad-do-it))

      (defvar pelm-org/basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")

      (setq org-capture-templates nil)

      (add-to-list 'org-capture-templates
                   `("t" "Task" entry
                     (file+headline ,(concat org-directory "/refile.org") "Inbox")
                     ,pelm-org/basic-task-template))

      ;; google calendar item
      (add-to-list 'org-capture-templates
                   `("g" "Google Calendar Entry " entry
                     (file (concat org-directory "/google.org")) "* %^{Task}\n %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n" :immediate-finish t) t)

      (add-to-list 'org-capture-templates
                   `("T" "Quick task" entry
                     (file+headline "~/.org-files/refile.org" "Inbox")
                     "* TODO %^{Task}\nSCHEDULED: %t\n"
                     :immediate-finish t))
      (add-to-list 'org-capture-templates
                   '("l" "Ledger entries"))

      (add-to-list 'org-capture-templates
                   `("lb" "Breakfast entry" plain
                     (file "~/.ledger/ledger.dat")
                     "%(org-read-date) * \"%^{Payee}\"
  Expenses:Food:Restaurant  %^{Amount}
  Assets:Alipay"
                     :immediate-finish t))




      (add-to-list 'org-capture-templates
                   `("i" "Interrupting task" entry
                      (file+headline "~/.org-files/refile.org" "Inbox")
                      "* STARTED %^{Task}"
                      :clock-in :clock-resume))

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

      ;; TODO test if need this ?
      (org-load-modules-maybe t)

      (defconst pelm-org-completed-date-regexp
        (concat "\\("
                "CLOSED: \\[%Y-%m-%d"
                "\\|"
                "- State \"\\(DONE\\|CANCELLED\\)\" * from .* \\[%Y-%m-%d"
                "\\|"
                "- State .* ->  *\"\\(DONE\\|CANCELLED\\)\" * \\[%Y-%m-%d"
                "\\)")
        "Matches any completion time stamp.")

      ;; Custom agenda command definitions -- start with a clean state
      (setq org-agenda-custom-commands nil)

      (add-to-list 'org-agenda-custom-commands
                   `("c" todo ""
                     ((org-agenda-overriding-header "Tasks to refile: ")
                      (org-agenda-files (list
                                         ,(concat org-directory "/refile.org")
                                         ,(concat org-directory "/mobileorg.org"))))))


      (add-to-list 'org-agenda-custom-commands
                   `("." "Today"
                     (
                      ;; Events.
                      (agenda ""
                              ((org-agenda-entry-types '(:timestamp :sexp))
                               (org-agenda-overriding-header
                                (concat "CALENDAR Today " (format-time-string "%a %d" (current-time))))
                               (org-agenda-span 'day)))
                      ;; Unscheduled new tasks (waiging to be prioritized and scheduled).
                      (todo "TODO"
                            ((org-agenda-overriding-header "COLLECTBOX (Unscheduled)")
                             (org-agenda-files (list ,(concat org-directory "/mobileorg.org")
                                                     ,(concat org-directory "/refile.org")
                                                     ))))

                      (tags-todo "TODO={STARTED\\|LEARN}"
                                 ((org-agenda-overriding-header "STARTED TASKS")
                                  (org-agenda-skip-function
                                   '(org-agenda-skip-entry-if 'deadline))))

                      ;; List of all TODO entries with deadline before today.
                      (tags-todo "DEADLINE<=\"<+0d>\"|SCHEDULED<=\"<+0d>\""
                                 ((org-agenda-overriding-header "OVERDUE")
                                  ;;(org-agenda-skip-function
                                  ;; '(org-agenda-skip-entry-if 'notdeadline))
                                  (org-agenda-sorting-strategy '(priority-down))))

                      (tags-todo "TODO={WAIT}"
                                 ((org-agenda-overriding-header "Waiting For")
                                  ;;(org-agenda-skip-function
                                  ;; '(org-agenda-skip-entry-if 'notdeadline))
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
                      (todo "TODO|DONE|CANCELLED" ; Includes repeated tasks (back in TODO).
                            ((org-agenda-overriding-header "COMPLETED TODAY")
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if
                                'notregexp
                                (format-time-string pelm-org-completed-date-regexp)))
                             (org-agenda-sorting-strategy '(priority-down)))))
                     ((org-agenda-format-date "")
                      (org-agenda-start-with-clockreport-mode nil))) t)

      ;; Weekly review
      (add-to-list 'org-agenda-custom-commands
                   `("w" "Weekly review" agenda ""
                     ((org-agenda-span 7)
                      (org-agenda-log-mode 1)) "~/Dropbox/agenda/this-week.html"))

      (add-to-list 'org-agenda-custom-commands
                   `("W" "Weekly review sans routines" agenda ""
                     ((org-agenda-span 7)
                      (org-agenda-log-mode 1)
                      (org-agenda-tag-filter-preset '("-routine"))) "~/Dropbox/agenda/this-week-nonroutine.html"))

      ;;FIXME: do i need this command ?
      ;; (add-to-list 'org-agenda-custom-commands
      ;;              '("2" "Bi-weekly review" agenda ""
      ;;                ((org-agenda-span 14)
      ;;                 (org-agenda-start-day "-1w") ;; FIXME: do it need this ?
      ;;                 (org-agenda-log-mode 1))))


      (add-to-list 'org-agenda-custom-commands '("g" . "Goto Tasks List...") t)
      (add-to-list 'org-agenda-custom-commands
                   `("gb" "Business" todo ""
                     (
                      (org-agenda-overriding-header "--")
                      (org-agenda-files (list ,(concat org-directory "/business.org")))
                      (org-agenda-view-columns-initially t))))

      (add-to-list 'org-agenda-custom-commands
                   '("gw" "Writing" tags-todo "@writing"
                     ((org-agenda-view-columns-initially t))))

      (add-to-list 'org-agenda-custom-commands
                   '("gp" "Work" tags-todo "@work"
                     (
                      (org-agenda-overriding-header "")
                      (org-agenda-view-columns-initially t))))

      (add-to-list 'org-agenda-custom-commands
                   '("gh" "Home" tags-todo "@home"
                     ((org-agenda-view-columns-initially t))))

      (add-to-list 'org-agenda-custom-commands
                   `("0" "Top 3 by context"
                     ,pelm-org/org-agenda-contexts
                     ((org-agenda-sorting-strategy '(priority-up effort-down))
                      (pelm-org/org-agenda-limit-items 3))))

      (add-to-list 'org-agenda-custom-commands
                   `(")" "All by context"
                      ,pelm-org/org-agenda-contexts
                      ((org-agenda-sorting-strategy '(priority-down effort-down))
                       (pelm-org/org-agenda-limit-items nil))))

      (add-to-list 'org-agenda-custom-commands
                     `("9" "Unscheduled top 3 by context"
                      ,pelm-org/org-agenda-contexts
                      ((org-agenda-skip-function 'pelm-org/org-agenda-skip-scheduled)
                       (org-agenda-sorting-strategy '(priority-down effort-down))
                       (pelm-org/org-agenda-limit-items 3))))

      (add-to-list 'org-agenda-custom-commands
                     `("(" "All unscheduled by context"
                      ,pelm-org/org-agenda-contexts
                      ((org-agenda-skip-function 'pelm-org/org-agenda-skip-scheduled)
                       (org-agenda-sorting-strategy '(priority-down effort-down))
                       )))

      ;; Show what happened today.
      (add-to-list 'org-agenda-custom-commands
                   '("d" "Timeline for today" ((agenda "" ))
                     ((org-agenda-ndays 1)
                      (org-agenda-show-log t)
                      (org-agenda-log-mode-items '(clock closed))
                      (org-agenda-clockreport-mode t)
                      (org-agenda-entry-types '()))))

      (add-to-list 'org-agenda-custom-commands
                   `("w" "Waiting for" todo "WAITING"))
      (add-to-list 'org-agenda-custom-commands
                   `("u" "Unscheduled tasks" tags-todo "-someday-TODO=\"SOMEDAY\"-TODO=\"DELEGATED\"-TODO=\"WAITING\"-project"
                     ((org-agenda-skip-function 'pelm-org/org-agenda-skip-scheduled)
                      (org-agenda-view-columns-initially t)
                      (org-tags-exclude-from-inheritance '("project"))
                      (org-agenda-overriding-header "Unscheduled TODO entries: ")
                      (org-columns-default-format "%50ITEM %TODO %3PRIORITY %Effort{:} %TAGS")
                      (org-agenda-sorting-strategy '(todo-state-up priority-down effort-up tag-up category-keep)))))

      (add-to-list 'org-agenda-custom-commands
                   `("U" "Unscheduled tasks outside projects" tags-todo "-project"
                     ((org-agenda-skip-function 'pelm-org/org-agenda-skip-scheduled)
                      (org-tags-exclude-from-inheritance nil)
                      (org-agenda-view-columns-initially t)
                      (org-agenda-overriding-header "Unscheduled TODO entries outside projects: ")
                      (org-agenda-sorting-strategy '(todo-state-up priority-down tag-up category-keep effort-down)))))

      (add-to-list 'org-agenda-custom-commands
                   `("P" "By priority"
                     ((tags-todo "+PRIORITY=\"A\"")
                      (tags-todo "+PRIORITY=\"B\"")
                      (tags-todo "+PRIORITY=\"\"")
                      (tags-todo "+PRIORITY=\"C\""))
                     ((org-agenda-prefix-format "%-10c %-10T %e ")
                      (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down)))))

      (add-to-list 'org-agenda-custom-commands
                   `("pp" tags "+project-someday-TODO=\"DONE\"-TODO=\"SOMEDAY\"-inactive"
                     ((org-tags-exclude-from-inheritance '("project"))
                      (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down)))))

      (add-to-list 'org-agenda-custom-commands
                   `("S" tags-todo "TODO=\"STARTED\""))

      (add-to-list 'org-agenda-custom-commands
                   `("2" "List projects with tasks" pelm-org/org-agenda-projects-and-tasks
                     "+PROJECT"
                     ((pelm-org/org-agenda-limit-items 3))))


      (defun pelm-org/org-agenda-project-agenda ()
        "Return the project headline and up to `pelm-org/org-agenda-limit-items' tasks."
        (save-excursion
          (let* ((marker (org-agenda-new-marker))
                 (heading
                  (org-agenda-format-item "" (org-get-heading) (org-get-category) nil))
                 (org-agenda-restrict t)
                 (org-agenda-restrict-begin (point))
                 (org-agenda-restrict-end (org-end-of-subtree 'invisible))
                 ;; Find the TODO items in this subtree
                 (list (org-agenda-get-day-entries (buffer-file-name) (calendar-current-date) :todo)))
            (org-add-props heading
                (list 'face 'defaults
                      'done-face 'org-agenda-done
                      'undone-face 'default
                      'mouse-face 'highlight
                      'org-not-done-regexp org-not-done-regexp
                      'org-todo-regexp org-todo-regexp
                      'org-complex-heading-regexp org-complex-heading-regexp
                      'help-echo
                      (format "mouse-2 or RET jump to org file %s"
                              (abbreviate-file-name
                               (or (buffer-file-name (buffer-base-buffer))
                                   (buffer-name (buffer-base-buffer))))))
              'org-marker marker
              'org-hd-marker marker
              'org-category (org-get-category)
              'type "tagsmatch")
            (concat heading "\n"
                    (org-agenda-finalize-entries list)))))

      (defun pelm-org/org-agenda-projects-and-tasks (match)
        "Show TODOs for all `org-agenda-files' headlines matching MATCH."
        (interactive "MString: ")
        (let ((todo-only nil))
          (if org-agenda-overriding-arguments
              (setq todo-only (car org-agenda-overriding-arguments)
                    match (nth 1 org-agenda-overriding-arguments)))
          (let* ((org-tags-match-list-sublevels
                  org-tags-match-list-sublevels)
                 (completion-ignore-case t)
                 rtn rtnall files file pos matcher
                 buffer)
            (when (and (stringp match) (not (string-match "\\S-" match)))
              (setq match nil))
            (when match
              (setq matcher (org-make-tags-matcher match)
                    match (car matcher) matcher (cdr matcher)))
            (catch 'exit
              (if org-agenda-sticky
                  (setq org-agenda-buffer-name
                        (if (stringp match)
                            (format "*Org Agenda(%s:%s)*"
                                    (or org-keys (or (and todo-only "M") "m")) match)
                          (format "*Org Agenda(%s)*" (or (and todo-only "M") "m")))))
              (org-agenda-prepare (concat "TAGS " match))
              (org-compile-prefix-format 'tags)
              (org-set-sorting-strategy 'tags)
              (setq org-agenda-query-string match)
              (setq org-agenda-redo-command
                    (list 'org-tags-view `(quote ,todo-only)
                          (list 'if 'current-prefix-arg nil `(quote ,org-agenda-query-string))))
              (setq files (org-agenda-files nil 'ifmode)
                    rtnall nil)
              (while (setq file (pop files))
                (catch 'nextfile
                  (org-check-agenda-file file)
                  (setq buffer (if (file-exists-p file)
                                   (org-get-agenda-file-buffer file)
                                 (error "No such file %s" file)))
                  (if (not buffer)
                      ;; If file does not exist, error message to agenda
                      (setq rtn (list
                                 (format "ORG-AGENDA-ERROR: No such org-file %s" file))
                            rtnall (append rtnall rtn))
                    (with-current-buffer buffer
                      (unless (derived-mode-p 'org-mode)
                        (error "Agenda file %s is not in `org-mode'" file))
                      (save-excursion
                        (save-restriction
                          (if org-agenda-restrict
                              (narrow-to-region org-agenda-restrict-begin
                                                org-agenda-restrict-end)
                            (widen))
                          (setq rtn (org-scan-tags 'pelm-org/org-agenda-project-agenda matcher todo-only))
                          (setq rtnall (append rtnall rtn))))))))
              (if org-agenda-overriding-header
                  (insert (org-add-props (copy-sequence org-agenda-overriding-header)
                              nil 'face 'org-agenda-structure) "\n")
                (insert "Headlines with TAGS match: ")
                (add-text-properties (point-min) (1- (point))
                                     (list 'face 'org-agenda-structure
                                           'short-heading
                                           (concat "Match: " match)))
                (setq pos (point))
                (insert match "\n")
                (add-text-properties pos (1- (point)) (list 'face 'org-warning))
                (setq pos (point))
                (unless org-agenda-multi
                  (insert "Press `C-u r' to search again with new search string\n"))
                (add-text-properties pos (1- (point)) (list 'face 'org-agenda-structure)))
              (org-agenda-mark-header-line (point-min))
              (when rtnall
                (insert (mapconcat 'identity rtnall "\n") ""))
              (goto-char (point-min))
              (or org-agenda-multi (org-agenda-fit-window-to-buffer))
              (add-text-properties (point-min) (point-max)
                                   `(org-agenda-type tags
                                                     org-last-args (,todo-only ,match)
                                                     org-redo-cmd ,org-agenda-redo-command
                                                     org-series-cmd ,org-cmd))
              (org-agenda-finalize)
              (setq buffer-read-only t)))))

      (add-to-list 'org-agenda-custom-commands
                   `("D" "Completed view"
                     (;; List of all TODO entries completed yesterday.
                      (todo "TODO|DONE|CANCELLED" ; includes repeated tasks (back in TODO)
                            ((org-agenda-overriding-header
                              (concat "YESTERDAY   "
                                      (format-time-string "%a %d" (current-time-ndays-ago 1))
                                      ;; #("__________________" 0 12 (face (:foreground "gray")))
                                      ))
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if
                                'notregexp
                                (format-time-string pelm-org-completed-date-regexp (current-time-ndays-ago 1))))
                             (org-agenda-sorting-strategy '(priority-down))))
                      ;; List of all TODO entries completed 2 days ago.
                      (todo "TODO|DONE|CANCELLED" ; includes repeated tasks (back in TODO)
                            ((org-agenda-overriding-header
                              (concat "2 DAYS AGO  "
                                      (format-time-string "%a %d" (current-time-ndays-ago 2))))
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if
                                'notregexp
                                (format-time-string pelm-org-completed-date-regexp (current-time-ndays-ago 2))))
                             (org-agenda-sorting-strategy '(priority-down))))
                      ;; List of all TODO entries completed 3 days ago.
                      (todo "TODO|DONE|CANCELLED" ; Includes repeated tasks (back in TODO).
                            ((org-agenda-overriding-header
                              (concat "3 DAYS AGO  "
                                      (format-time-string "%a %d" (current-time-ndays-ago 3))))
                             (org-agenda-skip-function
                              '(org-agenda-skip-entry-if
                                'notregexp
                                (format-time-string pelm-org-completed-date-regexp (current-time-ndays-ago 3))))
                             (org-agenda-sorting-strategy '(priority-down)))))
                     ((org-agenda-format-date "")
                      (org-agenda-start-with-clockreport-mode nil))) t)

      (defun current-time-ndays-ago (n)
        "Return the current time minus N days."
        (time-subtract (current-time) (days-to-time n)))

      (add-to-list 'org-agenda-custom-commands
                   '("rw" "Weekly review"
                     ((tags "CATEGORY={@Collect}&LEVEL=2|TODO={TODO}"
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

                      (todo "PROJECT"
                            ((org-agenda-overriding-header "PROJECT LIST")))

                      ;; FIXME we should show which tasks (don't) have CLOCK lines: archived vs. deleted.
                      (todo "DONE"
                            ((org-agenda-overriding-header
                              "Candidates to be archived")))

                      (todo "STARTED"
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

      (defun pelm-org/org-agenda-done (&optional arg)
        "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
        (interactive "P")
        (org-agenda-todo "DONE"))

      ;; Override the key definition for org-exit
      (define-key org-agenda-mode-map "x" 'pelm-org/org-agenda-done)

      ;; google calendar sync
      ;; FIXME: not working
      ;;(define-key org-agenda-mode-map "<SPC> gs" 'org-gcal-sync)

      (defun pelm-org/org-agenda-mark-done-and-add-followup ()
        "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
        (interactive)
        (org-agenda-todo "DONE")
        (org-agenda-switch-to)
        (org-capture 0 "t"))
      ;; Override the key definition
      (define-key org-agenda-mode-map "X" 'pelm-org/org-agenda-mark-done-and-add-followup)

      (autoload 'appt-check "appt")

      ;; Config support
      (defun pelm-org/mark-next-parent-tasks-todo ()
        "Visit each parent task and change STARTED states to TODO"
        (let ((mystate (or (and (fboundp 'state)
                                org-state)
                           (nth 2 (org-heading-components)))))
          (when (equal mystate "STARTED")
            (save-excursion
              (while (org-up-heading-safe)
                (when (member (nth 2 (org-heading-components)) (list "STARTED"))
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
            ;; Find the tags on the current task
            (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
                (org-clock-in '(16))
              (pelm-org/clock-in-organization-task-as-default)))))

      (defun pelm-org/punch-out ()
        (interactive)
        (setq pelm-org/keep-clock-running nil)
        (when (org-clock-is-active)
          (org-clock-out))
        (org-agenda-remove-restriction-lock))

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


      (defun pelm-org/clock-in-task-by-id (id)
        "Clock in a task by id"
        (save-restriction
          (widen)
          (org-with-point-at (org-id-find id 'marker)
            (org-clock-in '(16)))))

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

      ;;FIXME: temp disable helm for org-capture
      (defun kk/org-set-tags-no-helm (orig-func &rest args)
        "Run org-set-tags without helm."
        (if (boundp 'helm-mode)
            (let ((orig-helm-mode helm-mode))
              (unwind-protect
                  (progn
                    (helm-mode 0)
                    (apply orig-func args)
                    )
                (helm-mode (if orig-helm-mode 1 0))))
          (apply orig-func args)
          ))


      (advice-add 'org-capture :around 'kk/org-set-tags-no-helm)


      (org-babel-do-load-languages
       (quote org-babel-load-languages)
       (quote ((emacs-lisp . t)
               (dot . t)
               (ditaa . t)
               ;;(R . t)
               (ledger . t)
               ;;(haskell . t)
               ;;(python . t)
               ;;(ruby . t)
               ;;(scala . t)
               ;;(clojure . t)
               (sh . t)
               (js . t)
               ;; (java . t)
               (org . t)
               ;; (http . t)
               ;;(ipython . t)
               ;;(kotlin . t)
               (sql . t)
               (sqlite . t)
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


