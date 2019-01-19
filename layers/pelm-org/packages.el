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

(defconst pelm-org-packages '(
                              (org :location built-in)
                              orgtbl-aggregate
                              ob-kotlin))


(defun pelm-org/init-orgtbl-aggregate()
  (use-package orgtbl-aggregate
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "id" 'org-insert-dblock
        ))
    ))

(defun pelm-org/init-ob-kotlin()
  (use-package ob-kotlin
    :defer t))

(defun pelm-org/post-init-org ()
  (add-hook 'org-mode-hook (lambda () (spacemacs/toggle-line-numbers-off)) 'append)
  (with-eval-after-load 'org
    (progn
      (require 'org-crypt)
      (org-crypt-use-before-save-magic)
      (setq org-crypt-tag-matcher "secret")
      (setq org-tags-exclude-from-inheritance (quote ("secret")))

      (setq org-crypt-key nil)

      (spacemacs|disable-company org-mode)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "," 'org-priority)
      (require 'org-compat)
      (require 'org)
      ;; (add-to-list 'org-modules "org-habit")
      (add-to-list 'org-modules 'org-habit)
      (require 'org-habit)
      (setq org-export-babel-evaluate nil)
      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets '(("~/.org-files/gtd.org" :maxlevel . 3)
                                 ("~/.org-files/someday.org" :level . 1)))

      ;; config stuck project
      (setq org-stuck-projects
            '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

      (setq org-agenda-inhibit-startup t)   ;; ~50x speedup
      (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
      (setq org-agenda-window-setup 'current-window)
      (setq org-ctrl-k-protect-subtree t)                                   ;; Protect my subtrees!
      (setq org-blank-before-new-entry
            '((heading . nil) (plain-list-item . nil)))                       ;; Insert empty line before new headlines, but not before list item
      (setq org-footnote-auto-adjust t)                                     ;; Automatically renumber footnotes
      (setq org-goto-auto-isearch nil)
      (setq org-refile-allow-creating-parent-nodes t)                       ;; Allow interactive refile
      (setq org-special-ctrl-a/e t)                                         ;; Use Ctrl-a/e in a smarter way for Org.
      (setq org-log-done t)

      (setq org-todo-keywords
            '(
              (sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" )
              (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")))

      (setq org-todo-keyword-faces
            '(("IDEA" . (:foreground "GoldenRod" :weight bold))
              ("NEXT" . (:foreground "IndianRed1" :weight bold))
              ("STARTED" . (:foreground "OrangeRed" :weight bold))
              ("WAITING" . (:foreground "coral" :weight bold))
              ("CANCELED" . (:foreground "LimeGreen" :weight bold))
              ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
              ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))))

      (setq org-tag-persistent-alist
            '((:startgroup . nil)
              ("HOME" . ?h)
              ("RESEARCH" . ?r)
              ("TEACHING" . ?t)
              (:endgroup . nil)
              (:startgroup . nil)
              ("OS" . ?o)
              ("DEV" . ?d)
              ("WWW" . ?w)
              (:endgroup . nil)
              (:startgroup . nil)
              ("EASY" . ?e)
              ("MEDIUM" . ?m)
              ("HARD" . ?a)
              (:endgroup . nil)
              ("URGENT" . ?u)
              ("KEY" . ?k)
              ("BONUS" . ?b)
              ("noexport" . ?x)))

      (setq org-tag-faces
            '(
              ("HOME" . (:foreground "GoldenRod" :weight bold))
              ("RESEARCH" . (:foreground "GoldenRod" :weight bold))
              ("TEACHING" . (:foreground "GoldenRod" :weight bold))
              ("OS" . (:foreground "IndianRed1" :weight bold))
              ("DEV" . (:foreground "IndianRed1" :weight bold))
              ("WWW" . (:foreground "IndianRed1" :weight bold))
              ("URGENT" . (:foreground "Red" :weight bold))
              ("KEY" . (:foreground "Red" :weight bold))
              ("EASY" . (:foreground "OrangeRed" :weight bold))
              ("MEDIUM" . (:foreground "OrangeRed" :weight bold))
              ("HARD" . (:foreground "OrangeRed" :weight bold))
              ("BONUS" . (:foreground "GoldenRod" :weight bold))
              ("noexport" . (:foreground "LimeGreen" :weight bold))))

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
      (setq org-ditaa-jar-path "~/.spacemacs.d/layers/pelm-org/vendor/ditaa.jar")

      (require 'ob-sqlite)


      ;; latex related setup
(setq org-latex-create-formula-image-program 'imagemagick)
(setq org-latex-date-format "%Y-%m-%d")
(setq org-latex-default-class "ctexart")
(setq org-latex-pdf-process
      '(
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "rm -fr %b.out %b.log %b.tex auto"))
(add-to-list 'org-latex-packages-alist '("" "CJKutf8" t))

(setq org-format-latex-options
      '(:foreground "#0c0c0c"
                    :background "#ffffff"
                    :scale 2.2
                    :html-foreground "#0c0c0c" 
                    :html-background "#ffffff"
                    :html-scale 2.2
                    :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

(setq org-structure-template-alist
      (append '(("e" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
              org-structure-template-alist))

;; 这里强制使用"_{下标}"来定义一个下标。"^{上标}"来定义一个上标。
(setq org-export-with-sub-superscripts '{})
(setq org-use-sub-superscripts '{})

(setq eh-org-mathtoweb-file "~/.spacemacs.d/layers/pelm-org/vendor/mathtoweb.jar")
(setq org-latex-to-mathml-convert-command
      "java -jar %j -unicode -force -df %o %I"
      org-latex-to-mathml-jar-file
      eh-org-mathtoweb-file)

 (add-to-list 'org-latex-classes '("ctexart" "\\documentclass[11pt]{ctexart}
                                        [NO-DEFAULT-PACKAGES]
                                        \\usepackage[utf8]{inputenc}
                                        \\usepackage[T1]{fontenc}
                                        \\usepackage{fixltx2e}
                                        \\usepackage{graphicx}
                                        \\usepackage{longtable}
                                        \\usepackage{float}
                                        \\usepackage{wrapfig}
                                        \\usepackage{rotating}
                                        \\usepackage[normalem]{ulem}
                                        \\usepackage{amsmath}
                                        \\usepackage{textcomp}
                                        \\usepackage{marvosym}
                                        \\usepackage{wasysym}
                                        \\usepackage{amssymb}
                                        \\usepackage{booktabs}
                                        \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                                        \\tolerance=1000
                                        \\usepackage{listings}
                                        \\usepackage{xcolor}
                                        \\lstset{
                                        %行号
                                        numbers=left,
                                        %背景框
                                        framexleftmargin=10mm,
                                        frame=none,
                                        %背景色
                                        %backgroundcolor=\\color[rgb]{1,1,0.76},
                                        backgroundcolor=\\color[RGB]{245,245,244},
                                        %样式
                                        keywordstyle=\\bf\\color{blue},
                                        identifierstyle=\\bf,
                                        numberstyle=\\color[RGB]{0,192,192},
                                        commentstyle=\\it\\color[RGB]{0,96,96},
                                        stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                                        %显示空格
                                        showstringspaces=false
                                        }
                                        "
                                        ("\\section{%s}" . "\\section*{%s}")
                                        ("\\subsection{%s}" . "\\subsection*{%s}")
                                        ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                        ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                        ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (setenv "NODE_PATH"
              (concat
               (getenv "HOME") "/.npm-packages/lib/node_modules" ":"
               (getenv "HOME") "/.n/lib/node_modules" ":"
               "/usr/local/lib/node_modules" ":"
               (getenv "HOME") "/.org-files/node_modules"  ":"
               (getenv "NODE_PATH")))

      (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
      (add-hook 'org-mode-hook 'org-display-inline-images)
      (org-babel-do-load-languages
       'org-babel-load-languages
       '(
         ;; (latex . t)
         ;; (R . t)
         (shell . t)
         (calc . t)
         (sql . t )
         (sqlite . t)
         (lua . t)
         ;; (dot . t)
         (js . t)
         (kotlin . t)
         (org . t )
         (gnuplot . t)
         ;; (clojure . t )
         (restclient . t)
         (emacs-lisp . t)
         ;; (ipython . t)
         (python . t)
         (plantuml . t)
         (ditaa . t)
         ))


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
          (org-agenda-align-tags)))

      (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                     (file+headline "~/.org-files/inbox.org" "Tasks")
                                     "* TODO %i%?")
                                    ("c" "Contacts" entry (file "~/.org-files/contacts/contacts.org")
                                     "* %(org-contacts-template-name)
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE:
:WECHAT:
:BIRTHDAY:
:NOTE:
:END:")))

      (setq org-agenda-custom-commands
            '(("c" todo ""
               ((org-agenda-overriding-header "Tasks to refile: ")
                (org-agenda-files (list
                                   (concat org-agenda-dir "/inbox.org")
                                   (concat org-agenda-dir "/mobileorg.org")))))
              ("." "Today"
               (
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

              ("G" "GTD Block Agenda"
               ((todo "STARTED")
                (tags-todo "URGENT")
                (todo "NEXT"))
               ((org-agenda-prefix-format "[ ] %T: ")
                (org-agenda-with-colors nil)
                (org-agenda-compact-blocks t)
                (org-agenda-remove-tags t)
                (ps-number-of-columns 2)
                (ps-landscape-mode t))
               ("~/Desktop/next-actions.txt"))

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
               ((org-agenda-ndays 7)
                (org-agenda-start-on-weekday 0)
                (org-agenda-time-grid nil)
                (org-agenda-repeating-timestamp-show-all t)
                (org-agenda-entry-types '(:timestamp :sexp))))))

      (add-hook 'org-after-todo-statistics-hook 'pelm/org-summary-todo)
      (define-key org-mode-map (kbd "s-p") 'org-priority)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode "tl" 'org-toggle-link-display)
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

;;; packages.el ends here
