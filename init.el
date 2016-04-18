(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/")
   dotspacemacs-configuration-layers
   '(
     octave
     ruby
     sql
     vimscript
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t)
     spacemacs-ivy
     erc
     vinegar
   ;;  twitter
     emacs-lisp
     plantuml
     (org :variables
          org-enable-github-support t)
     ;;spell-checking
     git
     github
     markdown
     yaml
     (ibuffer :variables ibuffer-group-buffers-by nil)
     ;; (clojure
     ;;  :variables clojure-enable-fancify-symbols t)

     (shell :variables
            shell-default-term-shell "/usr/local/bin/zsh"
            shell-default-shell 'multiterm
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     syntax-checking
     version-control
     osx
     javascript
     java
     scala
     swift
     (python :variables
             python-enable-yapf-format-on-save t)
     react

     evil-commentary
     (colors :variables
             colors-enable-rainbow-identifiers nil )

     finance
     (elfeed :variables
             url-queue-timeout 30
             elfeed-enable-web-interface nil
             rmh-elfeed-org-files (list "~/.spacemacs.d/pelm-feed/feeds.org"))

     restclient
     search-engine
     (mu4e :variables
           mu4e-account-alist t
           mu4e-installation-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu/mu4e")
     fasd
     imenu-list
     ;;; just for fun
     ;;xkcd
     ;;typing-games
     ;;org-ipython
     ;;stack-exchange
     ;; play with
     ;;evernote
     ;; Personal Layers
     pelm-org
     pelm-blog
     pelm-misc
     pelm-ibuffer
     pelm-erc
     pelm-mail
     pelm-kotlin
     ;;pelm-slack
          )

   dotspacemacs-additional-packages '(key-chord ox-reveal nameless elfeed-org groovy-mode keyfreq org-clock-convenience buttercup)

   dotspacemacs-excluded-packages '(julia-mode  toc-org )
   dotspacemacs-delete-orphan-packages t)

  ;; (when
  ;;     (spacemacs/system-is-mac)
  ;;   (append dotspacemacs-configuration-layers '(spotify)))

  )

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 10
   dotspacemacs-check-for-update t
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '(recents bookmarks projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(monokai spacemacs-dark  spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("PragmataPro"
                               :size 18
                               :weight normal
                               :width normal
                               :powerline-scale 1.2)
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

   ;; remove the 4m from shell
   system-uses-terminfo nil

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
   git-magit-status-fullscreen nil
   magit-refresh-status-buffer t
   magit-commit-show-diff nil
   magit-revert-buffers t

   magit-repository-directories '(
                                  "~/.emacs.d/"
                                  "~/src/geek/Androidorg/"
                                  "~/.spacemacs.d/"
                                  "~/src/work/pacer_android/"
                                  "~/src/work/pacer_groups/"
                                  "~/src/work/mandian_server/"
                                  "~/src/personal/yep8.org/"
                                  "~/.zprezto/"
                                  )

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
   '(
     ;;("1\\.0\\.0" "#syl20bnr/spacemacs" "#eggcaker/emacs-hubot")
     ;;("irc.gitter.im" "#syl20bnr/spacemacs" "#eggcaker/emacs-hubot")
     ("irc.gitter.im"  "#eggcaker/emacs-hubot")
     ;;("localhost" "#动动健身" "#动动大集合")
     ;; ("freenode\\.net" "#org-mode")
     )


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

  (setq eclim-eclipse-dirs "/Applications/Eclipse.app/Contents/Eclipse"
        eclim-executable "/Applications/Eclipse.app/Contents/Eclipse/eclim")

  (global-git-commit-mode t)
  (push '(baidu
          :name "Baidu - 百度"
          :url "https://www.baidu.com/s?wd=%s")
        search-engine-alist)

  (push '(ciba
          :name "iCIBA - 词霸"
          :url "http://iciba.com/%s")
        search-engine-alist)

  (defun set-font (english chinese english-size chinese-size)
    (set-face-attribute
     'default nil :font (format "%s:pixelsize=%d" english english-size))
    (dolist
        (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font
       (frame-parameter nil 'font) charset (font-spec :family chinese :size
                                                      chinese-size))))
  (when (spacemacs/system-is-mac)
    (set-font "PragmataPro" "Source Han Sans SC" 18 20))

  (when (spacemacs/system-is-linux)
    (set-font "Source Code Pro" "Droid Sans Fallback" 18 20))


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

  (add-hook 'java-mode-hook (lambda ()
                              (setq c-basic-offset 2
                                    tab-width 2
                                    indent-tabs-mode t)))

  (global-company-mode)
  (turn-off-show-smartparens-mode)
  (setq powerline-default-separator 'arrow)

  ;; groovy for gradle file
  (add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))


    ;; clojure fancify symbols
    (setq clojure-enable-fancify-symbols t)

    (setq js2-include-node-externs t)
    (setq js2-include-browser-externs t)
    (setq js2-include-global-externs t)

    (setq display-time-mode t)
    (setq-default line-spacing 10)
    ;;(setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" ))
    (setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))

    ;;(setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▲" "●" ))
    (setq ledger-post-amount-alignment-column 68)
    (setq org-clock-persist-file "~/.emacs.d/.cache/org-clock-save.el")

    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))

    (setq yas-indent-line (quote none)) ;; do not auto indent snippet
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
      (erc-tls :server "irc.gitter.im" :port "6697" :nick "eggcaker"
               :password pelm/gitter-pwd :full-name pelm/full-name)
      (erc :server "irc.freenode.net" :port "6667" :nick "eggcaker"
           :password pelm/irc-pwd :full-name pelm/full-name)
      ;;(erc :server "localhost" :port "6667" :nick "eggcaker" :password "" :full-name "eggcaker") ;; local irc
      )

    ;; slack
    ;; (slack-register-team
    ;;  :name "Yep8"
    ;;  :default t
    ;;  :client-id pelm/slack-client-id
    ;;  :client-secret pelm/slack-client-secret
    ;;  :token pelm/slack-token
    ;;  :subscribed-channels '(review general))

    (defun pelm-shell/describe-random-interactive-function ()
      (interactive)
      "Show the documentation for a random interactive function.
Consider only documented, non-obsolete functions."
      (let (result)
        (mapatoms
         (lambda (s)
           (when (and (commandp s)
                      (documentation s t)
                      (null (get s 'byte-obsolete-info)))
             (setq result (cons s result)))))
        (describe-function (elt result (random (length result))))))

    (evil-leader/set-key "oh" 'pelm-shell/describe-random-interactive-function)

   ;; test the key freq
   (setq keyfreq-excluded-commands
         '(self-insert-command
           abort-recursive-edit
           forward-char
           backward-char
           previous-line
           next-line))
   (keyfreq-mode 1)
   (keyfreq-autosave-mode 1)

   (require 'org-clock-convenience)

   (defun dfeich/org-agenda-mode-fn ()
     (define-key org-agenda-mode-map
       (kbd "<S-up>") #'org-clock-convenience-timestamp-up)
     (define-key org-agenda-mode-map
       (kbd "<S-down>") #'org-clock-convenience-timestamp-down)
     (define-key org-agenda-mode-map
       (kbd "o") #'org-clock-convenience-fill-gap))
   (add-hook 'org-agenda-mode-hook #'dfeich/org-agenda-mode-fn)


    ;; Load local
    (when (file-exists-p "~/.local.el")
      (load "~/.local.el"))

    ;; Load temporary test code if exist
    (when (file-exists-p "~/Desktop/test.el")
      (load "~/Desktop/test.el"))

    )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("/Users/eggcaker/.org-files/bookmarks.org" "/Users/eggcaker/.org-files/books.org" "/Users/eggcaker/.org-files/business.org" "/Users/eggcaker/.org-files/contacts.org" "/Users/eggcaker/.org-files/emacs.org" "/Users/eggcaker/.org-files/geek.org" "/Users/eggcaker/.org-files/google.org" "/Users/eggcaker/.org-files/habits.org" "/Users/eggcaker/.org-files/mobileorg.org" "/Users/eggcaker/.org-files/notes.org" "/Users/eggcaker/.org-files/personal.org" "/Users/eggcaker/.org-files/refile.org" "/Users/eggcaker/.org-files/work.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
