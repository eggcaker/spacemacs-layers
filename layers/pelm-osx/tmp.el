
(defvar counsel-mac-apps-alist nil
  "List of data located in /usr/share/applications.")

(defvar counsel-mac-apps-faulty nil
  "List of faulty data located in /usr/share/applications.")

(defun counsel-mac-apps-list ()
  (let ((files
         (delete
          ".." (delete
                "." (file-expand-wildcards "/Applications/*.app")))))
    (dolist (file (cl-set-difference files (append (mapcar 'car counsel-mac-apps-alist)
                                                   counsel-mac-apps-faulty)
                                     :test 'equal))
      (with-temp-buffer
        (insert-file-contents (expand-file-name file "/usr/share/applications"))
        (let (name comment exec)
          (goto-char (point-min))
          (if (re-search-forward "^Name *= *\\(.*\\)$" nil t)
              (setq name (match-string 1))
            (error "File %s has no Name" file))
          (goto-char (point-min))
          (when (re-search-forward "^Comment *= *\\(.*\\)$" nil t)
            (setq comment (match-string 1)))
          (goto-char (point-min))
          (when (re-search-forward "^Exec *= *\\(.*\\)$" nil t)
            (setq exec (match-string 1)))
          (if (and exec (not (equal exec "")))
              (add-to-list
               'counsel-mac-apps-alist
               (cons (format "% -45s: %s%s"
                             (propertize exec 'face 'font-lock-builtin-face)
                             name
                             (if comment
                                 (concat " - " comment)
                               ""))
                     file))
            (add-to-list 'counsel-mac-apps-faulty file))))))
  counsel-mac-apps-alist)

(defun counsel-mac-app-action-default (desktop-shortcut)
  "Launch DESKTOP-SHORTCUT."
  (call-process-shell-command
   (format "open %s" (file-name-nondirectory desktop-shortcut))))

(defun counsel-mac-app-action-file (desktop-shortcut)
  "Launch DESKTOP-SHORTCUT with a selected file."
  (let* ((entry (rassoc desktop-shortcut counsel-mac-apps-alist))
         (short-name (and entry
                          (string-match "\\([^ ]*\\) " (car entry))
                          (match-string 1 (car entry))))
         (file (and short-name
                    (read-file-name
                     (format "Run %s on: " short-name)))))
    (if file
        (call-process-shell-command
         (format "open %s %s"
                 (file-name-nondirectory desktop-shortcut)
                 file))
      (user-error "cancelled"))))

(defun counsel-mac-app ()
  "Launch a Mac application"
  (interactive)
  (ivy-read "Open a application: " (counsel-mac-apps-list)
            :action #'counsel-mac-app-action-default
            :caller 'counsel-mac-app))
