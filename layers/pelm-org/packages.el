;;; packages.el --- pelm-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2014-2016 eggcaker
;;
;; Author: guanghui <eggcaker@gmail.com>
;; URL: https://github.com/eggcaker/spacemacs-layers
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst pelm-org-packages
	'((org :location built-in)
		 ;;org-pomodoro
		 deft
		 ;; org-tree-slide
		 ;; ox-reveal
		 ;; worf
		 ;; org-download
		 ))

(defun pelm-org/post-init-org-pomodoro ()
	(progn
		;; (add-hook 'org-pomodoro-finished-hook '(lambda () (pelm/growl-notification "Pomodoro Finished" "☕️ Have a break!" t)))
		;; (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (pelm/growl-notification "Short Break" "🐝 Ready to Go?" t)))
		;; (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (pelm/growl-notification "Long Break" " 💪 Ready to Go?" t)))
		))

(defun pelm-org/post-init-org ()
	(add-hook 'org-mode-hook (lambda () (spacemacs/toggle-line-numbers-off)) 'append)
	(with-eval-after-load 'org
		(progn
			(spacemacs|disable-company org-mode)
			(spacemacs/set-leader-keys-for-major-mode 'org-mode
				"," 'org-priority)
			(require 'org-compat)
			(require 'org)
			;; (add-to-list 'org-modules "org-habit")
			(add-to-list 'org-modules 'org-habit)
			(require 'org-habit)

			(setq org-refile-use-outline-path 'file)
			(setq org-outline-path-complete-in-steps nil)
			(setq org-refile-targets '(("~/.org-files/gtd.org" :maxlevel . 3)
										  ("~/.org-files/someday.org" :level . 1)
										  ("~/.org-files/tickler.org" :maxlevel . 2)))

			;; config stuck project
			(setq org-stuck-projects
				'("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

			(setq org-agenda-inhibit-startup t)   ;; ~50x speedup
			(setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
	 		(setq org-agenda-window-setup 'current-window)
			(setq org-ctrl-k-protect-subtree t)                                   ;; Protect my subtrees!
			(setq org-blank-before-new-entry
				'((heading . ni) (plain-list-item . nil)))                       ;; Insert empty line before new headlines, but not before list item
			(setq org-footnote-auto-adjust t)                                     ;; Automatically renumber footnotes
			(setq org-goto-auto-isearch nil)
			(setq org-refile-allow-creating-parent-nodes t)                       ;; Allow interactive refile
			(setq org-special-ctrl-a/e t)                                         ;; Use Ctrl-a/e in a smarter way for Org.
			(setq org-log-done t)

			(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

			;; org-refile to find a task
			(defun pelm-org--get-visible-buffers ()
				(let* ((cur-mode 'org-mode))
					(delq nil
						(mapcar
							(lambda (buffer)
								(when (and (equal cur-mode (buffer-local-value 'major-mode buffer))
										  (get-buffer-window buffer))
									`(,(buffer-file-name buffer) :maxlevel . 3)))
							(buffer-list)))))

			(defun pelm-org-goto-task ()
				(interactive)
				(if current-prefix-arg
					(call-interactively #'org-refile)
					(let* ((visible-org-files (pelm-org--get-visible-buffers))
							  (org-refile-targets visible-org-files))
						(call-interactively #'org-refile))))

			(spacemacs/set-leader-keys "oj" 'pelm-org-goto-task)

			;; Change task state to STARTED when clocking in
			(setq org-clock-in-switch-to-state "STARTED")
			;; Save clock data and notes in the LOGBOOK drawer
			(setq org-clock-into-drawer t)
			;; Removes clocked tasks with 0:00 duration
			(setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line

			(setq org-tags-match-list-sublevels nil)

			(add-hook 'org-mode-hook '(lambda ()
										  ;; keybinding for editing source code blocks
										  ;; keybinding for inserting code blocks
										  (local-set-key (kbd "C-c i s")
											  'pelm/org-insert-src-block)))
			;; reset subtask
			(setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

			(setq org-plantuml-jar-path
				(expand-file-name "~/.spacemacs.d/layers/pelm-org/vendor/plantuml.jar"))
			(setq org-ditaa-jar-path "~/.spacemacs.d/layers/pelm-org/vendor/dijtaa.jar")

			(org-babel-do-load-languages
				'org-babel-load-languages
				'(
					 (shell . t)
					 (sql . t )
					 ;; (dot . t)
					 (js . t)
					 ;; (latex . t)
					 ;;(python . t)
					 (restclient . t)
					 (emacs-lisp . t)
					 (plantuml . t)
					 (ditaa . t)))


			(require 'ox-md nil t)
			;; copy from chinese layer
			(defadvice org-html-paragraph (before org-html-paragraph-advice
											  (paragraph contents info) activate)
				"Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
				(let* ((origin-contents (ad-get-arg 1))
						  (fix-regexp "[[:multibyte:]]")
						  (fixed-contents
							  (replace-regexp-in-string
								  (concat
									  "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
					(ad-set-arg 1 fixed-contents)))

			;; define the refile targets
			(setq org-agenda-files (list org-agenda-dir))

			(with-eval-after-load 'org-agenda
				(define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
				(spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
					"." 'spacemacs/org-agenda-transient-state/body)
				(add-hook 'org-finalize-agenda-hook 'place-agenda-tags)
				(defun place-agenda-tags ()
					"Put the agenda tags by the right border of the agenda window."
					(setq org-agenda-tags-column (- 4 (window-width)))
					(org-agenda-align-tags))
				)

			(setq org-capture-templates '(("t" "Todo [inbox]" entry
											  (file+headline "~/.org-files/inbox.org" "Tasks")
											  "* TODO %i%?")
											 ("T" "Tickler" entry
												 (file+headline "~/.org-files/tickler.org" "Tickler")
												 "* %i%? \n %U")))

			(setq org-agenda-custom-commands
				'(("c" todo ""
					  ((org-agenda-overriding-header "Tasks to refile: ")
						  (org-agenda-files (list
												(concat org-agenda-dir "/inbox.org")
												(concat org-agenda-dir "/mobileorg.org")))))
					 ("." "Today"
						 (
							 ;; Events.
							 (agenda ""
								 ((org-agenda-entry-types '(:timestamp :sexp))
									 (org-agenda-overriding-header
										 (concat "CALENDAR Today " (format-time-string "%a %d" (current-time))))
									 (org-agenda-span 'day)))
							 ;; (tags-todo "TODO={STARTED\\|LEARN}"
							 ;; 	 ((org-agenda-overriding-header "STARTED TASKS")
							 ;; 		 (org-agenda-skip-function
							 ;; 			 '(org-agenda-skip-entry-if 'deadline))))

							 (tags "fix_now"
								 ((org-agenda-overriding-header "DO IT NOW!")
									 (org-agenda-skip-function
										 '(org-agenda-skip-entry-if 'todo 'done))
									 (org-agenda-files '("~/Desktop/.pacer_android_bug.org"))))

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
							 (org-agenda-start-with-clockreport-mode nil)))


					 ("W" "Weekly Review"
						 ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
							 ;; type "l" in the agenda to review logged items
							 (stuck "") ;; review stuck projects as designated by org-stuck-projects
							 (todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
							 (todo "MAYBE") ;; review someday/maybe items
							 (todo "WAITING"))) ;; review waiting items

					 ("O" "Office block agenda"
						 ((agenda "" ((org-agenda-ndays 1)))
							 ;; limits the agenda display to a single day
							 (tags-todo "+PRIORITY=\"A\"")
							 (tags-todo "@office|@phone")
							 (tags "project+CATEGORY=\"pacer_server\"")
							 ;; limits the tag search to the file circuspeanuts.org
							 (todo "WAITING"))
						 ((org-agenda-compact-blocks t))) ;; options set here apply to the entire block

					 ("C" "Calendar" agenda ""
						 ((org-agenda-ndays 7)                          ;; [1]
							 (org-agenda-start-on-weekday 0)               ;; [2]
							 (org-agenda-time-grid nil)
							 (org-agenda-repeating-timestamp-show-all t)   ;; [3]
							 (org-agenda-entry-types '(:timestamp :sexp))))  ;; [4]

					 ("p"  "工作任务安排" tags-todo "@office"
						 ((org-agenda-files '("~/.org-files/gtd.org" "~/Desktop/.pacer_android_bug.org"))
							 (org-agenda-sorting-strategy '(priority-up effort-down)))
							 ("~/Desktop/gtd/work.html"))))

			(add-hook 'org-after-todo-statistics-hook 'pelm/org-summary-todo)
			;; used by pelm/org-clock-sum-today-by-tags

			(define-key org-mode-map (kbd "s-p") 'org-priority)
			(spacemacs/set-leader-keys-for-major-mode 'org-mode
				"tl" 'org-toggle-link-display)
			(define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)

			;; hack for org headline toc
			(defun org-html-headline (headline contents info)
				"Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
				(unless (org-element-property :footnote-section-p headline)
					(let* ((numberedp (org-export-numbered-headline-p headline info))
							  (numbers (org-export-get-headline-number headline info))
							  (section-number (and numbers
												  (mapconcat #'number-to-string numbers "-")))
							  (level (+ (org-export-get-relative-level headline info)
										 (1- (plist-get info :html-toplevel-hlevel))))
							  (todo (and (plist-get info :with-todo-keywords)
										(let ((todo (org-element-property :todo-keyword headline)))
											(and todo (org-export-data todo info)))))
							  (todo-type (and todo (org-element-property :todo-type headline)))
							  (priority (and (plist-get info :with-priority)
											(org-element-property :priority headline)))
							  (text (org-export-data (org-element-property :title headline) info))
							  (tags (and (plist-get info :with-tags)
										(org-export-get-tags headline info)))
							  (full-text (funcall (plist-get info :html-format-headline-function)
											 todo todo-type priority text tags info))
							  (contents (or contents ""))
							  (ids (delq nil
									   (list (org-element-property :CUSTOM_ID headline)
										   (org-export-get-reference headline info)
										   (org-element-property :ID headline))))
							  (preferred-id (car ids))
							  (extra-ids
								  (mapconcat
									  (lambda (id)
										  (org-html--anchor
											  (if (org-uuidgen-p id) (concat "ID-" id) id)
											  nil nil info))
									  (cdr ids) "")))
						(if (org-export-low-level-p headline info)
							;; This is a deep sub-tree: export it as a list item.
							(let* ((type (if numberedp 'ordered 'unordered))
									  (itemized-body
										  (org-html-format-list-item
											  contents type nil info nil
											  (concat (org-html--anchor preferred-id nil nil info)
												  extra-ids
												  full-text))))
								(concat (and (org-export-first-sibling-p headline info)
											(org-html-begin-plain-list type))
									itemized-body
									(and (org-export-last-sibling-p headline info)
										(org-html-end-plain-list type))))
							(let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
									 (first-content (car (org-element-contents headline))))
								;; Standard headline.  Export it as a section.
								(format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
									(org-html--container headline info)
									(org-export-get-reference headline info)
									(concat (format "outline-%d" level)
										(and extra-class " ")
										extra-class)
									(format "\n<h%d id=\"%s\">%s%s</h%d>\n"
										level
										preferred-id
										extra-ids
										(concat
											(and numberedp
												(format
													"<span class=\"section-number-%d\">%s</span> "
													level
													(mapconcat #'number-to-string numbers ".")))
											full-text)
										level)
									;; When there is no section, pretend there is an
									;; empty one to get the correct <div
									;; class="outline-...> which is needed by
									;; `org-info.js'.
									(if (eq (org-element-type first-content) 'section) contents
										(concat (org-html-section first-content "" info) contents))
									(org-html--container headline info)))))))

			)))

(defun pelm-org/post-init-ox-reveal ()
	(setq org-reveal-root "file:///Users/guanghui/.emacs.d/reveal-js"))

(defun pelm-org/init-org-tree-slide ()
	(use-package org-tree-slide
		:init
		(spacemacs/set-leader-keys "oto" 'org-tree-slide-mode)))


(defun pelm-org/init-org-download ()
	(use-package org-download
		:defer t
		:init
		(org-download-enable)))

(defun pelm-org/init-worf ()
	(use-package worf
		:defer t
		:init
		(add-hook 'org-mode-hook 'worf-mode)))

(defun pelm-org/post-init-deft ()
	(progn
		(setq deft-use-filter-string-for-filename t)
		(spacemacs/set-leader-keys-for-major-mode 'deft-mode "q" 'quit-window)
		(setq deft-recursive t)
		(setq deft-extension "org")
		(setq deft-directory deft-dir)))
;;; packages.el ends here
