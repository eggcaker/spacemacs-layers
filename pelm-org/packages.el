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

(defconst pelm-org-packages
  '(
    org
    org-ac
    gnuplot
    ox-reveal
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

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

      ;; set org agenda global
      (spacemacs/declare-prefix "o" "org")
      (spacemacs/set-leader-keys
        "oa" 'pelm-org/agenda-dwim
        "oo" 'org-agenda
        "ob" 'org-iswitchb
        "og" 'org-clock-goto
        "od" 'pelm-org/goto-diary
        "oc" 'org-capture
        "os" 'org-search-view
        "on" 'pelm-org/goto-notes
        "ow" 'pelm-org/goto-work
        "ot" 'pelm-org/todo-list
        "ov" 'pelm-org/tags-list

        ;; set punch in and out keys
         "oI" 'pelm-org/punch-in
         "oO" 'pelm-org/punch-out))
    :config
    (progn
      ;; set org specific keybindings
      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "R" 'org-agenda-refile))
                'append)

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


      ;; Agenda clock report parameters
      (setq org-agenda-clockreport-parameter-plist
            (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

      (setq org-clock-persist-file (concat spacemacs-cache-directory "org-clock-save.el"))
      (setq org-startup-with-inline-images t)
      (setq org-src-fontify-natively t)
      (setq org-ellipsis "⤵")
      (setq org-ditaa-jar-path "~/.emacs.d/private/pelm-org/vendor/ditaa.jar")
      (setq org-plantuml-jar-path "~/.emacs.d/private/pelm-org/vendor/plantuml.jar")

      (setq org-log-done t)

      (eval-after-load 'org-indent
        '(spacemacs|hide-lighter org-indent-mode))
      (setq org-startup-indented t)

      (let ((dir (configuration-layer/get-layer-property 'pelm-org :dir)))
        (setq org-export-async-init-file (concat dir "org-async-init.el")))


      (setq org-export-backends '(ascii beamer html latex md rss reveal))
      (setq org-directory "~/.org-files"
            org-default-notes-files (list (concat org-directory "/inbox.org"))
            org-mobile-directory "~/Dropbox/org"
            org-mobile-inbox-for-pull "~/.org-files/inbox.org")

      (defvar org-gtd-other-files '("~/src/personal/scrum/scrum.org"
                                    "~/src/personal/yep8.org/blog/index.org"))
      (setf org-agenda-files (cons "~/.org-files" org-gtd-other-files))
      ;; auto save org files
      (run-at-time "00:55" 3600 'org-save-all-org-buffers)

      (setq org-show-entry-below (quote ((default))))


      (defmacro spacemacs|org-emphasize (fname char)
        "Make function for setting the emphasis in org mode"
        `(defun ,fname () (interactive)
                (org-emphasize ,char)))

      (eval-after-load "org-agenda"
        '(progn
           (define-key org-agenda-mode-map "P" 'pelm-org/narrow-to-project)
           (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
           (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
           (define-key org-agenda-mode-map (kbd "SPC") spacemacs-default-map)))

      ;; options
      (setq org-log-done (quote time)
            org-log-into-drawer "LOGBOOK"
            org-alphabetical-lists t
            org-agenda-span 'day
            org-src-fontify-natively t
            org-use-fast-todo-selection t
            org-treat-S-cursor-todo-selection-as-state-change nil
            org-habit-preceding-days 7
            org-habit-graph-column 102
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
            org-enable-priority-commands nil
            org-src-preserve-indentation nil
            org-html-coding-system 'utf-8
            org-html-head-include-default-style nil
            org-html-head-include-scripts nil
            org-tags-exclude-from-inheritance (quote ("crypt"))
            org-startup-folded 'content
            ido-max-directory-size 100000
            org-list-demote-modify-bullet (quote (("+" . "-")
                                                  ("*" . "-")
                                                  ("1." . "-")
                                                  ("1)" . "-")))
            org-refile-targets (quote ((nil :maxlevel . 9)
                                       (org-agenda-files :maxlevel . 9))))

      ;; org-todo-keywords


      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "INPROGRESS(i)" "|" "DONE(d!/!)")
                    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" )))

            org-todo-keyword-faces
            (quote (("TODO" :foreground "red" :weight bold)
                    ("INPROGRESS" :foreground "blue" :weight bold)
                    ("DONE" :foreground "forest green" :weight bold)
                    ("WAITING" :foreground "orange" :weight bold)
                    ("HOLD" :foreground "magenta" :weight bold)
                    ("CANCELLED" :foreground "forest green" :weight bold)))
            ;; state change trigger
            org-todo-state-tags-triggers
            (quote (("CANCELLED" ("CANCELLED" . t))
                    ("WAITING" ("WAITING" . t))
                    ("HOLD" ("WAITING" . t) ("HOLD" . t))
                    (done ("WAITING") ("HOLD"))
                    ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("INPROGRESS" ("WAITING") ("CANCELLED") ("HOLD"))
                    ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

      (setq org-capture-templates
            (quote (("t" "Toto" entry (file (concat org-directory "/inbox.org")) "* TODO %?\n%U\n %a\n " :clock-in t :clock-resume t)
                    ("x" "CLI TODO" entry (file (concat org-directory "/inbox.org")) "* TODO %i\n%U" :immediate-finish t)
                    ("p" "Web Notes" entry (file+headline (concat org-directory "/notes.org") "Web Notes")
                     "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?" :empty-lines 1)
                    ("L" "Protocol Link" entry (file+headline (concat org-directory "/bookmarks.org") "Links") "* %? [[%:link][%:description]] \nCaptured On: %U")
                    ("c" "Contacts" entry (file "~/.org-files/contacts.org") "* %(org-contacts-template-name)
 :PROPERTIES:
 :EMAIL: %(org-contacts-template-email)
 :PHONE:
 :ALIAS:
 :NICKNAME:
 :IGNORE:
 :ICON:
 :NOTE:
 :ADDRESS:
 :BIRTHDAY:
 :END:"))))

      ;; Enable habit tracking (and a bunch of other modules)
      (setq org-modules
            (quote (org-bbdb
                    org-bibtex
                    org-crypt
                    org-gnus
                    org-id
                    org-info
                    org-jsinfo
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

      ;; Custom agenda command definitions
      (setq org-agenda-custom-commands
            (quote (
                    ("h" "Habits" tags-todo "STYLE=\"habit\""
                     ((org-agenda-overriding-header "Habits")
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
                    (" " "Agenda"
                     ((agenda "" nil)
                      (tags "REFILE"
                            ((org-agenda-overriding-header "Tasks to Refile")
                             (org-tags-match-list-sublevels nil)))
                      (tags-todo "-CANCELLED/!"
                                 ((org-agenda-overriding-header "Stuck Projects")
                                        ;(org-tags-match-list-sublevels 'indented)
                                  (org-agenda-skip-function 'pelm-org/skip-non-stuck-projects)))
                      (tags-todo "-INPROGRESS"
                                 ((org-agenda-overriding-header "Next Tasks")
                                  (org-agenda-skip-function 'pelm-org/skip-projects-and-habits-and-single-tasks)
                                  (org-agenda-todo-ignore-scheduled t)
                                  (org-agenda-todo-ignore-deadlines t)
                                  (org-tags-match-list-sublevels t)
                                  (org-agenda-sorting-strategy
                                   '(todo-state-down effort-up category-keep))))
                      (tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
                                 ((org-agenda-overriding-header "Backlogs")
                                  (org-agenda-skip-function 'pelm-org/skip-project-tasks-maybe)
                                  (org-agenda-todo-ignore-scheduled t)
                                  ;;(org-agenda-skip-function 'pelm-org/skip-habits)
                                  (org-agenda-todo-ignore-deadlines t)
                                  (org-agenda-sorting-strategy
                                   '(category-keep))))
                      (tags-todo "-CANCELLED/!"
                                 ((org-agenda-overriding-header "Projects")
                                  (org-agenda-skip-function 'pelm-org/skip-non-projects)
                                  (org-agenda-todo-ignore-scheduled 'future)
                                  (org-agenda-todo-ignore-deadlines 'future)
                                  (org-agenda-sorting-strategy
                                   '(category-keep))))
                      (tags-todo "-CANCELLED/!WAITING|HOLD"
                                 ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                                  (org-agenda-skip-function 'pelm-org/skip-stuck-projects)
                                  (org-tags-match-list-sublevels nil)
                                  (org-agenda-todo-ignore-scheduled 'future)
                                  (org-agenda-todo-ignore-deadlines 'future)))
                      (tags "-ARCHIVE/"
                            ((org-agenda-overriding-header "Tasks to Archive")
                             (org-agenda-skip-function 'pelm-org/skip-non-archivable-tasks))))
                     nil)
                    ("r" "Tasks to Refile" tags "REFILE"
                     ((org-agenda-overriding-header "Tasks to Refile")
                      (org-tags-match-list-sublevels nil)))
                    ("#" "Stuck Projects" tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function 'pelm-org/skip-non-stuck-projects)))
                    ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!INPROGRESS"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-agenda-skip-function 'pelm-org/skip-projects-and-habits-and-single-tasks)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
                    ("R" "Tasks" tags-todo "-REFILE-CANCELLED/!-HOLD-WAITING"
                     ((org-agenda-overriding-header "Tasks")
                      (org-agenda-skip-function 'pelm-org/skip-project-tasks-maybe)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
                    ("p" "Projects" tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Projects")
                      (org-agenda-skip-function 'pelm-org/skip-non-projects)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-todo-ignore-deadlines 'future)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
                    ("w" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING|HOLD"
                     ((org-agenda-overriding-header "Waiting and Postponed tasks"))
                     (org-agenda-skip-function 'pelm-org/skip-projects-and-habits)
                     (org-agenda-todo-ignore-scheduled 'future)
                     (org-agenda-todo-ignore-deadlines 'future))
                    ("A" "Tasks to Archive" tags "-ARCHIVE/"
                     ((org-agenda-overriding-header "Tasks to Archive")
                      (org-agenda-skip-function 'pelm-org/skip-non-archivable-tasks))))))


      ;; some functions
      ;; TODO: move out ??
      (defun pelm-org/goto-diary ()
        (interactive)
        (find-file org-agenda-diary-file))

      (defun pelm-org/goto-notes ()
        (interactive)
        (find-file org-default-notes-file))

      (defun pelm-org/goto-work ()
        (interactive)
        (find-file org-work-file))

      (defun pelm-org/todo-list ()
        "Show the todo list."
        (interactive)
        (org-agenda prefix-arg "t")
        (org-agenda-filter-apply '("-someday") 'tag))

      (defun pelm-org/tags-list ()
        "Show all tagged items."
        (interactive)
        (org-tags-view nil))

      (defun pelm-org/narrow-to-subtree-content ()
        "Narrow to the content of the subtree.  Excludes the heading line."
        (widen)
        (unless (org-at-heading-p) (org-back-to-heading))
        (org-narrow-to-subtree)
        (forward-line)
        (narrow-to-region (line-beginning-position) (point-max)))

      (defun pelm-org/subtree-content ()
        "Return the content of the subtree at point as a string."
        (save-excursion
          (save-restriction
            (org-narrow-to-subtree)
            (buffer-substring-no-properties (point-min) (point-max)))))

      (defun pelm-org/write-subtree-content (dest)
        "Write the contents of the subtree at point to a file at DEST."
        (interactive (list (ido-read-file-name "Write subtree to: " nil nil nil ".org")))
        (f-write-text (pelm-org/subtree-content) 'utf-8 dest)
        (when (called-interactively-p nil)
          (message "Subtree written to %s" dest)))

      (defun pelm-org/copy-subtree-to ()
        "Create a duplicate of the current subtree at the given heading."
        (interactive "*")
        (atomic-change-group
          (org-copy-subtree)
          (org-clone-subtree-with-time-shift 1 '(16))
          (call-interactively 'org-refile)))

      
;;; Custom keyboard commands

      (defun pelm-org/ctrl-c-ctrl-k (&optional n)
        "Kill subtrees, unless we're in a special buffer where it should cancel."
        (interactive "p")
        (if (s-starts-with? "*Org" (buffer-name))
            (org-kill-note-or-show-branches)
          (org-cut-subtree n)))

      (defun pelm-org/ctrl-c-ret ()
        "Call `org-table-hline-and-move' or `org-insert-todo-heading' dep. on context."
        (interactive)
        (cond
         ((org-at-table-p) (call-interactively 'org-table-hline-and-move))
         (t (call-interactively 'org-insert-todo-heading))))

      (defun pelm-org/agenda-dwim ()
        "Show the work agenda view if at work, otherwise the standard agenda."
        (interactive)
        (if (and (boundp 'org-work--at-work?) org-work--at-work?)
            (org-agenda current-prefix-arg "w")
          (org-agenda current-prefix-arg "A"))
        (delete-other-windows))

      
;;; Diary utils

      (defun calendar-nearest-to (target-dayname target-day target-month)
        "Non-nil if the current date is a certain weekday close to an anniversary.

TARGET-DAYNAME is the day of the week that we want to match,
 while TARGET-DAY and TARGET-MONTH are the anniversary."
        (let* ((dayname (calendar-day-of-week date))
               (target-date (list target-month target-day (calendar-extract-year date)))
               (days-diff (abs (- (calendar-day-number date)
                                  (calendar-day-number target-date)))))
          (and (= dayname target-dayname)
               (< days-diff 4))))

      (defun calendar-mondayised (target-day target-month)
        "Given anniversary with DAY and MONTH, return non-nil if:

- the given date is a weekday, or

- it is the Monday after the given date if it falls on a weekend."
        (if (memq (calendar-day-of-week date) '(6 0)) ; Sat or Sun
            (calendar-nearest-to 1 target-day target-month)

          (let ((m (calendar-extract-month date))
                (d (calendar-extract-day date)))
            (and (equal d target-day)
                 (equal m target-month)))) )

      (defun diary-limited-cyclic (recurrences interval m d y)
        "For use in emacs diary. Cyclic item with limited number of recurrences.
Occurs every INTERVAL days, starting on YYYY-MM-DD, for a total of
RECURRENCES occasions."
        (let ((startdate (calendar-absolute-from-gregorian (list m d y)))
              (today (calendar-absolute-from-gregorian date)))
          (and (not (cl-minusp (- today startdate)))
               (zerop (% (- today startdate) interval))
               (< (floor (- today startdate) interval) recurrences))))

      (defun calendar-easter-date (year)
        "Calculate the date for Easter Sunday in YEAR. Returns the date in the
Gregorian calendar, ie (MM DD YY) format."
        (let* ((century (1+ (/ year 100)))
               (shifted-epact (% (+ 14 (* 11 (% year 19))
                                    (- (/ (* 3 century) 4))
                                    (/ (+ 5 (* 8 century)) 25)
                                    (* 30 century))
                                 30))
               (adjusted-epact (if (or (= shifted-epact 0)
                                       (and (= shifted-epact 1)
                                            (< 10 (% year 19))))
                                   (1+ shifted-epact)
                                 shifted-epact))
               (paschal-moon (- (calendar-absolute-from-gregorian
                                 (list 4 19 year))
                                adjusted-epact)))
          (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

      (defun calendar-days-from-easter ()
        "When used in a diary sexp, this function will calculate how many days
are between the current date (DATE) and Easter Sunday."
        (- (calendar-absolute-from-gregorian date)
           (calendar-easter-date (calendar-extract-year date))))

      (autoload 'appt-check "appt")

      (defun pelm-org/diary-update-appt-on-save ()
        (save-restriction
          (save-window-excursion
            (org-agenda-to-appt t)
            (appt-check 'force))))

      
;;; Config support

      (defun pelm-org/display-links ()
        (interactive)
        (let ((bufname "*Org Links*"))
          (-if-let (buf (get-buffer bufname))
              (display-buffer buf)
            (with-current-buffer (find-file-noselect org-default-notes-file)
              (-when-let (mark (save-excursion (org-find-exact-headline-in-buffer "Links")))
                (clone-indirect-buffer bufname t)
                (goto-char (marker-position mark))
                (org-narrow-to-subtree)
                (org-content))))))


      (defun pelm-org/narrow-to-org-project ()
        (widen)
        (save-excursion
          (pelm-org/find-project-task)
          (pelm-org/narrow-to-org-subtree)))

      (defun pelm-org/narrow-to-project ()
        (interactive)
        (if (equal major-mode 'org-agenda-mode)
            (org-with-point-at (org-get-at-bol 'org-hd-marker)
              (pelm-org/narrow-to-org-project))
          (pelm-org/narrow-to-org-project)))


      (defun pelm-org/set-agenda-restriction-lock (arg)
        "Set restriction lock to current task subtree or file if prefix is specified"
        (interactive "p")
        (let* ((pom (or (org-get-at-bol 'org-hd-marker)
                        org-agenda-restrict-begin))
               (tags (org-with-point-at pom (org-get-tags-at))))
          (let ((restriction-type (if (equal arg 4) 'file 'subtree)))
            (save-restriction
              (cond
               ((equal major-mode 'org-agenda-mode)
                (org-with-point-at pom
                  (org-agenda-set-restriction-lock restriction-type)))
               ((and (equal major-mode 'org-mode) (org-before-first-heading-p))
                (org-agenda-set-restriction-lock 'file))
               (t
                (org-with-point-at pom
                  (org-agenda-set-restriction-lock restriction-type))))))))

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


      (defun pelm-org/show-org-agenda ()
        (interactive)
        (switch-to-buffer "*Org Agenda*")
        (delete-other-windows)
        (org-agenda-redo))


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

      (defun pelm-org/skip-non-stuck-projects ()
        "Skip trees that are not stuck projects"
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (if (pelm-org/is-project-p)
                (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                       (has-next (save-excursion
                                   (forward-line 1)
                                   (and (< (point) subtree-end)
                                        (re-search-forward "^\\*+ \\(INPROGRESS\\) " subtree-end t)))))
                  (if has-next
                      next-headline
                    nil)) ; a stuck project, has subtasks but no next task
              next-headline))))

      (defun pelm-org/skip-non-projects ()
        "Skip trees that are not projects"
        (pelm-org/list-sublevels-for-projects-indented)
        (if (save-excursion (pelm-org/skip-non-stuck-projects))
            (save-restriction
              (widen)
              (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                (if (pelm-org/is-project-p)
                    nil
                  subtree-end)))
          (org-end-of-subtree t)))

      (defun pelm-org/skip-habits ()
        "Skip the habits"
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((org-is-habit-p)
              subtree-end)
             (t
              nil)))))

      (defun pelm-org/skip-project-trees-and-habits ()
        "Skip trees that are projects"
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((pelm-org/is-project-p)
              subtree-end)
             ((org-is-habit-p)
              subtree-end)
             (t
              nil)))))

      (defun pelm-org/skip-projects-and-habits-and-single-tasks ()
        "Skip trees that are projects, tasks that are habits, single non-project tasks"
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (cond
             ((org-is-habit-p)
              next-headline)
             ((pelm-org/is-project-p)
              next-headline)
             ((and (pelm-org/is-task-p) (not (pelm-org/is-project-subtree-p)))
              next-headline)
             (t
              nil)))))

      (defun pelm-org/skip-project-tasks-maybe ()
        "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, INPROGRESS tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
        (save-restriction
          (widen)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (next-headline (save-excursion (or (outline-next-heading) (point-max))))
                 (limit-to-project (marker-buffer org-agenda-restrict-begin)))
            (cond
             ((pelm-org/is-project-p)
              next-headline)
             ((org-is-habit-p)
              subtree-end)
             ((and (not limit-to-project)
                   (pelm-org/is-project-subtree-p))
              subtree-end)
             ((and limit-to-project
                   (pelm-org/is-project-subtree-p)
                   (member (org-get-todo-state) (list "INPROGRESS")))
              subtree-end)
             (t
              nil)))))

      (defun pelm-org/skip-projects-and-habits ()
        "Skip trees that are projects and tasks that are habits"
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((pelm-org/is-project-p)
              subtree-end)
             ((org-is-habit-p)
              subtree-end)
             (t
              nil)))))

      (defun pelm-org/skip-non-subprojects ()
        "Skip trees that are not projects."
        (let ((next-headline (save-excursion (outline-next-heading))))
          (if (pelm-org/is-subproject-p)
              nil
            next-headline)))

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

      (defun pelm-org/skip-stuck-projects ()
        "Skip trees that are not stuck projects."
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (if (pelm-org/is-project-p)
                (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                       (has-next ))
                  (save-excursion
                    (forward-line 1)
                    (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ INPROGRESS" subtree-end t))
                      (unless (member "WAITING" (org-get-tags-at))
                        (setq has-next t))))
                  (if has-next
                      nil
                    next-headline)) ; a stuck project, has subtasks but no next task
              nil))))


      (defun pelm-org/org-auto-exclude-function (tag)
        "Automatic task exclusion in the agenda with / RET"
        (and (cond
              (
               (string= tag "hold") t)
              )
             (concat "-" tag)))

      (setq org-agenda-auto-exclude-function 'pelm-org/org-auto-exclude-function)



      (defun pelm-org/hide-other ()
        (interactive)
        (save-excursion
          (org-back-to-heading 'invisible-ok)
          (org-shifttab)
          (org-reveal)
          (org-cycle)))

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

      (defun pelm-org/skip-non-stuck-projects ()
        "Skip trees that are not stuck projects"
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (if (pelm-org/is-project-p)
                (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                       (has-next (save-excursion
                                   (forward-line 1)
                                   (and (< (point) subtree-end)
                                        (re-search-forward "^\\*+ \\(INPROGRESS\\) " subtree-end t)))))
                  (if has-next
                      next-headline
                    nil)) ; a stuck project, has subtasks but no next task
              next-headline))))

      (defun pelm-org/skip-non-projects ()
        "Skip trees that are not projects"
        (pelm-org/list-sublevels-for-projects-indented)
        (if (save-excursion (pelm-org/skip-non-stuck-projects))
            (save-restriction
              (widen)
              (let ((subtree-end (save-excursion (org-end-of-subtree t))))
                (if (pelm-org/is-project-p)
                    nil
                  subtree-end)))
          (org-end-of-subtree t)))

      (defun pelm-org/skip-project-trees-and-habits ()
        "Skip trees that are projects"
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((pelm-org/is-project-p)
              subtree-end)
             ((org-is-habit-p)
              subtree-end)
             (t
              nil)))))

      (defun pelm-org/skip-projects-and-habits-and-single-tasks ()
        "Skip trees that are projects, tasks that are habits, single non-project tasks"
        (save-restriction
          (widen)
          (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
            (cond
             ((org-is-habit-p)
              next-headline)
             ((pelm-org/is-project-p)
              next-headline)
             ((and (pelm-org/is-task-p) (not (pelm-org/is-project-subtree-p)))
              next-headline)
             (t
              nil)))))

      (defun pelm-org/skip-project-tasks-maybe ()
        "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, INPROGRESS tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
        (save-restriction
          (widen)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (next-headline (save-excursion (or (outline-next-heading) (point-max))))
                 (limit-to-project (marker-buffer org-agenda-restrict-begin)))
            (cond
             ((pelm-org/is-project-p)
              next-headline)
             ((org-is-habit-p)
              subtree-end)
             ((and (not limit-to-project)
                   (pelm-org/is-project-subtree-p))
              subtree-end)
             ((and limit-to-project
                   (pelm-org/is-project-subtree-p)
                   (member (org-get-todo-state) (list "INPROGRESS")))
              subtree-end)
             (t
              nil)))))

      (defun pelm-org/skip-projects-and-habits ()
        "Skip trees that are projects and tasks that are habits"
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((pelm-org/is-project-p)
              subtree-end)
             ((org-is-habit-p)
              subtree-end)
             (t
              nil)))))

      (defun pelm-org/skip-non-subprojects ()
        "Skip trees that are not projects."
        (let ((next-headline (save-excursion (outline-next-heading))))
          (if (pelm-org/is-subproject-p)
              nil
            next-headline)))
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
                '(lambda () (org-defkey org-agenda-mode-map "N" 'pelm-org/narrow-to-subtree))
                'append)

      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "U" 'pelm-org/narrow-up-one-level))
                'append)


      (add-hook 'org-agenda-mode-hook
                '(lambda () (org-defkey org-agenda-mode-map "\C-c\C-x<" 'pelm-org/set-agenda-restriction-lock))
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
      )))


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


