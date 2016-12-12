;;; packages.el --- pelm-org Layer packages File for Spacemacs

(setq pelm-org-packages '(org))

(defun pelm-org/post-init-org ()
  (setq-default
   org-tags-column -80
   org-startup-indented t
   org-clock-into-drawer "LOGBOOK"
   org-log-into-drawer "LOGBOOK"
   org-startup-align-all-tables t
   org-footnote-auto-adjust t
   org-footnote-auto-label 'confirm
   org-M-RET-may-split-line
   '((headline . nil) (item . nil) (table . nil))
   org-agenda-restore-windows-after-quit t
   org-agenda-window-setup 'other-window
   org-directory "~/.org-files"
   org-default-notes-file "~/.org-files/capture.org"
   org-agenda-files '("~/.org-files/agenda.org")
   org-catch-invisible-edits 'show-and-error
   org-list-demote-modify-bullet '(("-" . "*") ("*" . "+") ("+" . "-"))
   org-list-allow-alphabetical t
   org-todo-keywords
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


   org-todo-keyword-faces
   '((("TODO" :foreground "red" :weight bold)
     ("NEXT" :foreground "blue" :weight bold)
     ("STARTED" :foreground "yellow" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)))

   org-capture-templates
   '(("t" "Tasks")
     ("tg" "General" entry (file+headline "" "Tasks")
      "* TODO %?\n%i\n%U"
      :empty-lines 1)
     ("tl" "Location" entry (file+headline "" "Tasks")
      "* TODO %?\n%i\n%U\n%a"
      :empty-lines 1)
     ("n" "Notes")
     ("ng" "General" entry (file+headline "" "Notes")
      "* %?\n%i\n%U"
      :empty-lines 1)
     ("nl" "Location" entry (file+headline "" "Notes")
      "* %?\n%i\n%U\n%a"
      :empty-lines 1))




   ))
