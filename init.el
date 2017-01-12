(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation t
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/")
   dotspacemacs-configuration-layers
   '(
     lua
     octave
     spacemacs-purpose
     ;;csv
     ;;selectric
     ;;octave
     ;;nginx
     ;;octave
     ;;ruby
    ;; ess
     sql
     ;;vimscript
     (auto-completion
      :variables
      auto-completion-return-key-behavior 'complete
      auto-completion-tab-key-behavior 'cycle
      auto-completion-private-snippets-directory "~/.spacemacs.d/snippets/"
      auto-completion-complete-with-key-sequence "jj"
      auto-completion-complete-with-key-sequence-delay 0.1
      auto-completion-enable-snippets-in-popup t
      auto-completion-enable-help-tooltip t
      auto-completion-enable-sort-by-usage t
      :disabled-for org erc)

     ivy
     ;erc
    ;; vinegar
   ;;  twitter
     emacs-lisp
     ;;plantuml
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-reveal-js-support t)
     ;;spell-checking
     git
     github
     (markdown :variables markdown-live-preview-engine 'vmd)
     ;;dockerfile
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
     ;;jabber
     osx
     javascript
     ;;java
     ;;scala
     ;;swift
     (python :variables
             python-enable-yapf-format-on-save t)
     react
     evil-commentary
     ;; (colors :variables
     ;;         colors-enable-rainbow-identifiers nil )

     ;;finance
     (elfeed :variables
             url-queue-timeout 30
             elfeed-enable-web-interface nil
             rmh-elfeed-org-files (list "~/.spacemacs.d/pelm-feed/feeds.org"))

     restclient
     ;;(restclient :variables
      ;;           restclient-use-org t)

     search-engine
     (mu4e :variables
           mu4e-account-alist t
           mu4e-installation-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu/mu4e")
     ;;fasd
     imenu-list
     ;;; just for fun
     ;;xkcd
     ;;typing-games
     ;;org-ipython
     ;;stack-exchange
     ;; play with
     ;;evernote
     ;; Personal Layers
    ;; pelm-blog
     pelm-misc
     pelm-org
     ;;pelm-ibuffer
    ;; pelm-erc
     ;;pelm-mail
     ;;pelm-kotlin
     ;;pelm-slack
          )

   dotspacemacs-additional-packages
   '(key-chord
     ox-reveal
     nameless
     elfeed-org
     groovy-mode
     keyfreq
     org-clock-convenience
     buttercup
     editorconfig
     evil-embrace
     counsel-osx-app)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '()
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 10
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory 'emacs-version
   dotspacemacs-helm-use-fuzzy 'source
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '( bookmarks (recents . 15) projects)
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(monokai spacemacs-dark darkokai material )
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
   dotspacemacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 10
   dotspacemacs-helm-resize t
   dotspacemacs-helm-no-header t
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-transient-state t
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-close-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "ack" "grep")
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
   custom-file  (concat dotspacemacs-directory "custom.el")

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
   git-commit-summary-max-length 60
   git-magit-status-fullscreen t
   magit-refresh-status-buffer t
   magit-commit-show-diff nil
   magit-revert-buffers t

   magit-repository-directories '(
                                  "~/src/work/pacer_android/"
                                  "~/src/geek/wechathubot/"
                                  ;;"~/.spacemacs.d/"
                                  ;;"~/src/work/pacer_groups/"
                                  ;"~/src/work/mandian_server/"
                                  )

   ;; Flycheck
   avy-all-windows 'all-frames
   flycheck-check-syntax-automatically '(save mode-enabled)

   ;; Avy
   avy-all-windows 'all-frames

   ;; Spaceline
   ;;spaceline-buffer-encoding-abbrev-p nil
   ;;spaceline-version-control-p nil

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
     ;;("irc.gitter.im"  "#eggcaker/emacs-hubot")
     ("irc.gitter.im"  "#mandian/ci")
     ;;("localhost" "#动动健身" "#动动大集合")
     ;; ("freenode\\.net" "#org-mode")
     )

   ;; Theme modifications
   theming-modifications '()
   ))

(defun dotspacemacs/user-config ()
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

  ;; Diminish
  (spacemacs|diminish holy-mode)
  (spacemacs|diminish hybrid-mode)
  (spacemacs|diminish which-key-mode)
  (spacemacs|diminish evil-mc-mode)
  (spacemacs|diminish helm-gtags-mode)
  (spacemacs|diminish ggtags-mode)
  (with-eval-after-load 'emoji-cheat-sheet-plus
    (diminish 'emoji-cheat-sheet-plus-display-mode))
  (with-eval-after-load 'racer
    (diminish 'racer-mode))
  (with-eval-after-load 'command-log-mode
    (diminish 'command-log-mode))

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

  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'calculator-mode 'emacs)
  (push 'term-mode evil-escape-excluded-major-modes)
  (evil-define-key 'emacs term-raw-map (kbd "C-c") 'term-send-raw)

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
    (setq smerge-command-prefix "m")
    (setq js2-include-node-externs t)
    (setq js2-include-browser-externs t)
    (setq js2-include-global-externs t)

    (setq display-time-mode t)
    (setq-default line-spacing 10)
    ;;(setq org-bullets-bullet-list '("☯" "☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" ))
    ;;(setq org-bullets-bullet-list '("✙" "♱" "♰" "☥" "✞" "✟" "✝" "†" "✠" "✚" "✜" "✛" "✢" "✣" "✤" "✥"))

    (setq org-bullets-bullet-list '("✺" "✹" "✸" "✷" "✶" "✭" "✦" "■" "▲" "●" ))
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
      ;;(erc :server "irc.freenode.net" :port "6667" :nick "eggcaker"
      ;;    :password pelm/irc-pwd :full-name pelm/full-name)
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

   (evil-leader/set-key "aa" 'counsel-osx-app)

   (defmacro bb|wrap-func (func)
     (let ((advice-name (intern (format "%s--advice" func)))
           (target-name (intern (format "%s/%s" func system-name))))
       `(progn
          (defun ,advice-name (&rest args)
            (when (fboundp ',target-name)
              (apply ',target-name args)))
          (advice-add ',func :after ',advice-name))))

   (bb|wrap-func dotspacemacs/layers)
   (bb|wrap-func dotspacemacs/init)
   (bb|wrap-func dotspacemacs/user-init)
   (bb|wrap-func dotspacemacs/user-config)


    ;; Load local
    (when (file-exists-p "~/.local.el")
      (load "~/.local.el"))

    )

(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yapfify yaml-mode xterm-color ws-butler window-numbering which-key wgrep web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit swift-mode sql-indent spacemacs-theme spaceline smex smeargle slim-mode shell-pop scss-mode sass-mode reveal-in-osx-finder restart-emacs rainbow-mode rainbow-identifiers rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort pug-mode plantuml-mode pip-requirements persp-mode pcre2el pbcopy paradox ox-twbs ox-reveal ox-gfm osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-plus-contrib org-gcal org-download org-clock-convenience org-bullets open-junk-file ob-restclient ob-http neotree nameless multi-term mu4e-maildirs-extension mu4e-alert move-text monokai-theme mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode launchctl keyfreq key-chord json-mode js2-refactor js-doc jabber ivy-purpose ivy-hydra insert-shebang info+ indent-guide ido-vertical-mode ibuffer-projectile hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make groovy-mode google-translate golden-ratio gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md flycheck-pos-tip flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks engine-mode emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies dumb-jump diff-hl darkokai-theme cython-mode counsel-projectile counsel-osx-app company-web company-tern company-statistics company-shell company-restclient company-anaconda column-enforce-mode color-identifiers-mode coffee-mode clean-aindent-mode buttercup auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
