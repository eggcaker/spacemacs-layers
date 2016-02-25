(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/")
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'cycle
                      auto-completion-complete-with-key-sequence nil
                      auto-completion-private-snippets-directory "~/.spaemacs.d/snippets/")
     ;;games
     ;; better-defaults
     erc
     emacs-lisp
     plantuml
     (org :variables
          org-enable-github-support t)
     git
     github
     markdown
     yaml
     (ibuffer :variables ibuffer-group-buffers-by nil)
     ;(clojure
     ; :variables clojure-enable-fancify-symbols t
     ; )
     ;swift
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     ;;spell-checking
     syntax-checking
     version-control
     osx
     ;; tern
     javascript
     react
     ;;finance
     ;;deft
     evil-commentary
     (elfeed :variables
             url-queue-timeout 30
             elfeed-enable-web-interface nil
             rmh-elfeed-org-files (list "~/.spacemacs.d/pelm-feed/feeds.org"))

     ;; (chinese
     ;; :variables
     ;; chinese-default-input-method 'wubi
     ;; chinese-enable-youdao-dict t)
     restclient
     search-engine
     ;;xkcd
     ;;typing-games
     ;;org-ipython

     (mu4e :variables
           mu4e-account-alist t
           mu4e-installation-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu/mu4e")

     ;; Personal Layers 
     pelm-org
     pelm-blog
     pelm-misc
     pelm-ibuffer
     pelm-erc
     ;;notmuch
     ;;stack-exchange
     ;; play with
     ;;evernote
     ;;fasd
     )

   dotspacemacs-additional-packages '(key-chord ox-reveal nameless elfeed-org groovy-mode)

   dotspacemacs-excluded-packages '(julia-mode  toc-org )
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 10
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '(recents bookmarks projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(monokai spacemacs-dark  spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Source Code Pro"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab t
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 10
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-which-key-delay 1.0
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-close-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'changed))

(defun dotspacemacs/user-init ()
  (setq-default

   ;; Miscellaneous
   vc-follow-symlinks t
   ring-bell-function 'ignore
   require-final-newline t
   indent-tabs-mode nil
   system-time-locale "C"
   paradox-github-token t
   open-junk-file-find-file-function 'find-file

   ;; Backups
   backup-directory-alist `((".*" . ,temporary-file-directory))
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   make-backup-files nil

   ;; Evil
   evil-shift-round nil
   evil-want-C-i-jump t

   ;; Whitespace mode
   whitespace-style '(face tabs tab-mark newline-mark)
   whitespace-display-mappings
   '((newline-mark 10 [172 10])
     (tab-mark 9 [9655 9]))

   ;; Smartparens
   sp-highlight-pair-overlay nil
   sp-highlight-wrap-overlay nil
   sp-highlight-wrap-tag-overlay nil

   ;; Magit
   magit-popup-show-common-commands nil

   ;; Flycheck
   flycheck-check-syntax-automatically '(save mode-enabled)

   ;; Avy
   avy-all-windows 'all-frames

   ;; Spaceline
   spaceline-buffer-encoding-abbrev-p nil
   spaceline-version-control-p nil

   ;; Shell
   shell-default-term-shell "/usr/local/bin/zsh"

   ;; Web
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-style-padding 2
   web-mode-script-padding 2
   web-mode-code-indent-offset 2
   web-mode-markup-indent-offset 2

   ;; Js
   js-indent-level 2
   js2-basic-offset 2
   js-switch-indent-offset 2
   js2-indent-switch-body 2

   ;; flycheck
   flycheck-jshintrc "~/.jshintrc"
   flycheck-jscsrc "~/.jscsrc"
   flycheck-eslintrc "~/.eslintrc"

   ;; js2-mode
   js2-basic-offset 2
   css-indent-offset 2

   ;; Emacs Lisp
   nameless-global-aliases
   '(("sm" . "spacemacs")
     ("dsm" . "dotspacemacs")
     ("cfl" . "configuration-layer")
     ("sl" . "spaceline")
     ("eip" . "evil-indent-plus"))

   nameless-discover-current-name nil
   nameless-prefix ""
   nameless-separator nil


   ;; IRC
   erc-autojoin-channels-alist
   '(("1\\.0\\.0" "#syl20bnr/spacemacs") ; Gitter
     ("irc.gitter.im" "#syl20bnr/spacemacs")
     ("freenode\\.net" "#org-mode"))

   ;; Theme modifications
   theming-modifications
   '((monokai
      ;; Font locking
      (font-lock-comment-face :slant italic)
      (font-lock-string-face :slant italic)
      (font-lock-doc-face :slant italic)
      (font-lock-keyword-face :weight bold)
      (font-lock-builtin-face :foreground "#ff9eb8")
      (font-lock-warning-face :underline nil)
      (web-mode-html-attr-value-face
       :inherit font-lock-string-face :foreground nil)
      (web-mode-html-attr-name-face
       :inherit font-lock-variable-name-face :foreground nil)
      (web-mode-html-tag-face
       :inherit font-lock-builtin-face :foreground nil :weight bold)
      (web-mode-html-tag-bracket-face
       :inherit web-mode-html-tag-face :foreground nil)
      (web-mode-comment-face
       :inherit font-lock-comment-face :foreground nil)

      ;; Modeline
      (mode-line :box (:color "#999999" :line-width 1 :style released-button))
      (powerline-active1 :box (:color "#999999" :line-width 1 :style released-button)
                         :background "#5a5a5a")
      (powerline-active2 :box (:color "#999999" :line-width 1 :style released-button))
      (mode-line-inactive :box (:color "#666666" :line-width 1 :style released-button))
      (powerline-inactive1 :box (:color "#666666" :line-width 1 :style released-button))
      (powerline-inactive2 :box (:color "#666666" :line-width 1 :style released-button))
      (helm-prefarg :foreground "PaleGreen")

      ;; Flycheck
      (flycheck-fringe-error :background nil)
      (flycheck-fringe-warning :background nil)
      (flycheck-fringe-info :background nil)

      ;; Other
      (company-tooltip-annotation
       :foreground "#ff9eb8" :background "#49483e")
      (company-tooltip-annotation-selection :background "#66d9ef")
      (erc-timestamp-face
       :inherit font-lock-comment-face :foreground nil)
      (evil-search-highlight-persist-highlight-face
       :background "#fc5fef" :foreground "#000000")
      (region :background "#998f84")
      (spacemacs-transient-state-title-face :background nil :foreground nil :inherit font-lock-warning-face)
      (term :foreground nil :background nil)))))

(defun dotspacemacs/user-config ()

  (push '(baidu
          :name "Baidu - 百度"
          :url "https://www.baidu.com/s?wd=%s")
        search-engine-alist)


  (defun pelm/node-eval ()
    (interactive)
    (let ((debug-on-error t) (start 1) (end 1))
      (cond
       (mark-active
        (setq start (point))
        (setq end (mark)))
       (t
        (setq start (point-min))
        (setq end (point-max))))
      (call-process-region
       start end     ; seems the order does not matter
       "node"        ; node.js
       nil           ; don't delete region
       "*node.js eval*"     ; output buffer
       nil)          ; no redisply during output
      (message "Region or buffer evaluated!")
      (setq deactivate-mark nil)))

  (defun offlineimap-get-password (host port)
    (require 'netrc)
    (let* ((netrc (netrc-parse (expand-file-name "~/.authinfo.gpg")))
           (hostentry (netrc-machine netrc host port port)))
      (when hostentry (netrc-get hostentry "password"))))


  (setq-default
   puml-plantuml-jar-path "/Users/eggcaker/.spacemacs.d/pelm-org/vendor/plantuml.jar"
   org-plantuml-jar-path "~/.spacemacs.d/pelm-org/vendor/plantuml.jar"
   js2-strict-trailing-comma-warning nil
   js2-highlight-external-variables nil
   truncate-lines t
   company-idle-delay 0.0
   tab-width 2
   js2-basic-offset 2
   css-indent-offset 2)

  (global-company-mode)
  (turn-off-show-smartparens-mode)
  (setq powerline-default-separator 'arrow)

  (evil-leader/set-key (kbd "omr") 'recentf-open-files)

  ;; Evalute JavaScript
  (evil-leader/set-key (kbd "oe") 'pelm/node-eval)

  ;; groovy for gradle file
  (add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

  ;; Buffer related (should be under o directly)
  (evil-leader/set-key (kbd "ol") 'avy-goto-line)
  (evil-leader/set-key (kbd "oc") 'avy-copy-line)
  (evil-leader/set-key (kbd "or") 'avy-copy-region)
  (evil-leader/set-key (kbd "oh") 'avy-goto-char-in-line)
  (evil-leader/set-key (kbd "on") 'spacemacs/new-empty-buffer)

  ;; mu4e config
  (setq mu4e-account-alist
        '(("gmail"
           ;; Under each account, set the account-specific variables you want.
           (mu4e-sent-messages-behavior delete)
           (mu4e-sent-folder "/Gmail/[Gmail].Sent Mail")
           (mu4e-drafts-folder "/Gmail/[Gmail].Drafts")
           (mu4e-trash-folder "/Gmail/[Gmail].Trash")
           (mu4e-refile-folder "/Gmail/[Gmail].Archive")

           (user-mail-address "eggcaker@gmail.com")
           (user-full-name "eggcaker"))
          ;; ("qq"
          ;;  (mu4e-sent-messages-behavior sent)
          ;;  (mu4e-sent-folder "/qq/Sent Items")
          ;;  (mu4e-drafts-folder "/qq/Drafts")
          ;;  (user-mail-address "eggcaker@qq.com")
          ;;  (user-full-name "eggcaker"))

          ))
  (mu4e/mail-account-reset)
  (setq mu4e-maildir "~/.mails"
        mu4e-trash-folder "/Trash"
        mail-user-agent 'mu4e-user-agent
        mu4e-refile-folder "/Archive"
        mu4e-get-mail-command "offlineimap"
        mu4e-update-interval nil
        mu4e-view-prefer-html t
        mu4e-compose-signature "eggcaker\nIt is easy to make things.\nIt is hard to make things simple."
        mu4e-html2text-command "w3m -T text/html"
        mu4e-view-show-images t
        mu4e-display-image t
        mu4e-view-show-addresses t)


  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;;; Mail directory shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/Gmail/INBOX" . ?g)
          ("/Gmail/PacerHealth" . ?p)))

  ;; Custom marks
  (setq mu4e-headers-new-mark              '("N" . "✉")
        mu4e-headers-empty-parent-prefix '("-" . "○")
        mu4e-headers-first-child-prefix '("\\" . "┗━❯")
        mu4e-headers-has-child-prefix '("+" . "┗◉")
        mu4e-headers-duplicate-prefix      '("=" . "⚌")
        mu4e-headers-default-prefix  '("|" . "┃"))


  ;;; Bookmarks
  (setq mu4e-bookmarks
        `(("flag:unread AND NOT flag:trashed AND NOT maildir:/Gmail/[Gmail].Spam" "Unread messages" ?u)
          ("date:today..now AND NOT maildir:/Gmail/[Gmail].Spam" "Today's messages" ?t)
          ("date:7d..now" "Last 7 days" ?w)
          ("mime:image/*" "Messages with images" ?p)
          (,(mapconcat 'identity
                       (mapcar
                        (lambda (maildir)
                          (concat "maildir:" (car maildir)))
                        mu4e-maildir-shortcuts) " OR ")
           "All inboxes" ?i)
          ("flag:unread AND NOT flag:trashed AND maildir:/Gmail/[Gmail].Spam"     "Unread spam"               ?s)

          ))

  ;; Send mail setup
  ;; SMTP setup
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        starttls-use-gnutls t)
  ;; Personal info
  (setq user-full-name "Tongzhu, Zhang")
  (setq user-mail-address "eggcaker@gmail.com")
  ;; gmail setup
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-smtp-user "eggcaker@gmail.com")

  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))


    ;; clojure fancify symbols
    (setq clojure-enable-fancify-symbols t)

    (setq js2-include-node-externs t)
    (setq js2-include-browser-externs t)
    (setq js2-include-global-externs t)

    ;; key-chord to exit insert mode
    (require 'key-chord)
    (setq key-chord-one-key-delay 0.16)
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
    (key-chord-define-global "UU"     'org-capture)

    (setq-default line-spacing 10)
    ;;(setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" ))
    (setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))

    ;;(setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▲" "●" ))
    (setq ledger-post-amount-alignment-column 68)
    (setq org-clock-persist-file "~/.emacs.d/.cache/org-clock-save.el")

    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))


    ;; ;; define the search engine
    ;; (setq search-engine-alist
    ;;       '(            (duck-duck-go
    ;;          :name "Duck Duck Go"
    ;;          :url "https://duckduckgo.com/?q=%s")
    ;;         (google
    ;;          :name "Google"
    ;;          :url "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
    ;;         (github
    ;;          :name "Github"
    ;;          :url "https://github.com/search?ref=simplesearch&q=%s")
    ;;         (stack-overflow
    ;;          :name "Stack Overflow"
    ;;          :url "https://stackoverflow.com/search?q=%s"
    ;;          )))


    (use-package nameless
      :defer t
      :init
      (progn
        (add-hook 'emacs-lisp-mode-hook 'nameless-mode-from-hook)
        (spacemacs|add-toggle nameless
          :status nameless-mode
          :on (nameless-mode)
          :off (nameless-mode -1)
          :evil-leader-for-mode (emacs-lisp-mode . "o:"))))

    ;; IRC
    (spacemacs|define-custom-layout "@ERC"
      :binding "E"
      :body
      (erc-tls :server "irc.gitter.im" :port "6667" :nick "eggcaker"
               :password pelm/gitter-pwd :full-name pelm/full-name)
      (erc :server "irc.freenode.net" :port "6667" :nick "eggcaker" :full-name pelm/full-name))

    ;; Load local
    (when (file-exists-p "~/.local.el")
      (load "~/.local.el")))


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
