;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
    ;; Base distribution to use. This is a layer contained in the directory
    ;; `+distribution'. For now available distributions are `spacemacs-base'
    ;; or `spacemacs'. (default 'spacemacs)
    dotspacemacs-distribution 'spacemacs
    ;; Lazy installation of layers (i.e. layers are installed only when a file
    ;; with a supported type is opened). Possible values are `all', `unused'
    ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
    ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
    ;; lazy install any layer that support lazy installation even the layers
    ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
    ;; installation feature and you have to explicitly list a layer in the
    ;; variable `dotspacemacs-configuration-layers' to install it.
    ;; (default 'unused)
    dotspacemacs-enable-lazy-installation 'unused
    ;; If non-nil then Spacemacs will ask for confirmation before installing
    ;; a layer lazily. (default t)
    dotspacemacs-ask-for-lazy-installation t
    ;; If non-nil layers with lazy install support are lazy installed.
    ;; List of additional paths where to look for configuration layers.
    ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
    dotspacemacs-configuration-layer-path '()
    ;; List of configuration layers to load.
    dotspacemacs-configuration-layers
    '(vimscript
      clojure
       react
       csv
      html
       ;;csv
       ;;selectric
       ;;octave
       ;;nginx
       ;;octave
       ;;ruby
       ;; ess
       sql
       ;;docker
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
         :disabled-for org erc sh)

       ivy
       (javascript :variables
         tern-command '("node" "/usr/local/bin/tern"))
       ;;erc
       ;; vinegar
       ;; twitter
       emacs-lisp
       common-lisp
       plantuml
       (org :variables
         org-enable-github-support t
         org-enable-org-journal-support t
         org-enable-bootstrap-support t
         org-enable-reveal-js-support t)
       ;;spell-checking
       git
       ;;github
       (markdown :variables markdown-live-preview-engine 'vmd)
       ;;dockerfile
       yaml
       (ibuffer :variables ibuffer-group-buffers-by nil)
       (clojure
         :variables clojure-enable-fancify-symbols t)


       (shell :variables
         shell-default-term-shell "/usr/local/bin/zsh"
         shell-default-shell 'eshell
         shell-enable-smart-eshell t
         shell-default-height 30
         shell-protect-eshell-prompt nil
         shell-default-position 'right)
       shell-scripts
       syntax-checking
       version-control
       ;;jabber
       ;;restructuredtext
       ;;docker
       java
       ;;vimscript
       ;;nginx
       ;;ruby
       ;;html
       ;;lua
       ;;(typescript :variables
       ;; typescript-fmt-on-save t)
       ;;csv
       osx
       ;;swift
       ;; ipython-notebook
                                        ;org-ipython
       (python :variables
         python-fill-column 110
         python-sort-imports-on-save t
         python-enable-yapf-format-on-save t)
       ;;react
       evil-commentary
       ;; (colors :variables
       ;;         colors-enable-rainbow-identifiers nil )

       ;;finance
       (elfeed :variables
         url-queue-timeout 30
         elfeed-enable-web-interface nil
         rmh-elfeed-org-files (list "~/.spacemacs.d/pelm-feed/feeds.org"))

       ;; restclient
       (restclient :variables
         restclient-use-org t)

       ;; search-engine
       ;; (mu4e :variables
       ;;       mu4e-account-alist t
       ;;       mu4e-installation-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu/mu4e")
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
       pelm-misc
       pelm-org
       pelm-org-trello
       pelm-contact
       pelm-music
       ;;pelm-blog
       ;;pelm-finance
       ;;google-calendar
       ;;spotify
       ;;slack
       ;;pelm-ibuffer
       ;; pelm-erc
       ;;pelm-mail
       pelm-kotlin
       ;;pelm-slack
       )
    ;; List of additional packages that will be installed without being
    ;; wrapped in a layer. If you need some configuration for these
    ;; packages, then consider creating a layer. You can also put the
    ;; configuration in `dotspacemacs/user-config'.
    dotspacemacs-additional-packages '(key-chord ox-reveal nameless elfeed-org groovy-mode keyfreq org-clock-convenience buttercup editorconfig evil-embrace counsel-osx-app)
    ;; A list of packages that cannot be updated.
    dotspacemacs-frozen-packages '()
    ;; A list of packages that will not be installed and loaded.
    dotspacemacs-excluded-packages '()
    ;; Defines the behaviour of Spacemacs when installing packages.
    ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
    ;; `used-only' installs only explicitly used packages and uninstall any
    ;; unused packages as well as their unused dependencies.
    ;; `used-but-keep-unused' installs only the used packages but won't uninstall
    ;; them if they become unused. `all' installs *all* packages supported by
    ;; Spacemacs and never uninstall them. (default is `used-only')
    dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5
   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the lastest
   ;; version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil
   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil
   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.

    dotspacemacs-default-font '("CodeNewRoman Nerd Font"
                       ;; dotspacemacs-default-font '("Source Code Pro"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non-nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non-nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non-nil, the paste transient-state is enabled. While enabled, pressing
   ;; `p' several times cycles through the elements in the `kill-ring'.
   ;; (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non-nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"
   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil
   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil
   ))

(defun dotspacemacs/user-init ()


;; (setq configuration-layer-elpa-archives
;;    '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
;;       ("org-cn"   . "http://elpa.emacs-china.org/org/")
;;      ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")))

  (defun set-indent (n)
    (setq-default
      evil-shift-width n
      tab-width n
      default-tab-width n
      standard-indent n
      c-basic-offset n
      js-indent-level n
      js2-basic-offset n
      javascript-indent-level n
      css-indent-offset n
      prolog-indent-width n
      coffee-tab-width n
      yaml-indent-offset n
      web-mode-markup-indent-offset n
      web-mode-css-indent-offset n
      web-mode-code-indent-offset n
      web-mode-markup-indent-offset n
      web-mode-css-indent-offset n
      web-mode-code-indent-offset n
      web-mode-attr-indent-offset n))
  (set-indent 2)

  (setq-default
    python-indent-offset 2
    python-indent 2)

  (defun set-tab-width (n)
    (dolist (var '(evil-shift-width
                    default-tab-width
                    tab-width
                    c-basic-offset
                    cmake-tab-width
                    coffee-tab-width
                    cperl-indent-level
                    css-indent-offset
                    elixir-smie-indent-basic
                    enh-ruby-indent-level
                    erlang-indent-level
                    javascript-indent-level
                    js-indent-level
                    js2-basic-offset
                    js3-indent-level
                    lisp-indent-offset
                    livescript-tab-width
                    mustache-basic-offset
                    nxml-child-indent
                    perl-indent-level
                    puppet-indent-level
                    yaml-indent-offset
                    python-indent-offset
                    ruby-indent-level
                    rust-indent-offset
                    scala-indent:step
                    sgml-basic-offset
                    sh-basic-offset
                    web-mode-code-indent-offset
                    web-mode-css-indent-offset
                    web-mode-markup-indent-offset))
      (set (make-local-variable var) n)))
  (set-tab-width 2)
  (add-hook 'python-mode-hook (lambda () (set-indent 4)))

  (setq-default
    ;; remove the 4m from shell
    system-uses-terminfo nil
    exec-path-from-shell-check-startup-files nil
    google-translate-default-target-language "zh-CN"
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
                                    ;; "~/.spacemacs.d/"
                                    "~/src/work/pacer_android/"
                                    "~/.dotfiles"
                                    ;;"~/src/work/pacer_groups/"
                                    ;;"~/src/work/mandian_server/"
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
    explicit-shell-file-name "/usr/local/bin/zsh"
    shell-file-name "zsh"
    explicit-bash.exe-args '("--noediting" "--login" "-i")

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
  ;; (add-hook 'python-mode-hook
  ;;   (lambda ()
  ;;     (setq indent-tabs-mode nil)
  ;;     (setq python-indent 4)
  ;;     (setq evil-auto-indent nil)
  ;;     (setq tab-width 4))
  ;;   (untabify (point-min) (point-max)))

  ;; emms
;;  (setq emms-player-mpd-server-port "6600")
 ;; (emms-player-mpd-connect)

  (setenv "SHELL" shell-file-name)
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

  ;; Load local.el first
  (when (file-exists-p "~/.local.el")
    (load "~/.local.el"))

  (global-git-commit-mode t)
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

  (setq flycheck-flake8-maximum-line-length 110)
  (setq spaceline-org-clock-p t)
  (setq org-enable-org-journal-support t
    org-journal-dir "~/.journal/"
    org-journal-file-format "%Y-%m-%d")

  (setq calendar-holidays
    '(
       (holiday-fixed 1 1 "元旦")
       (holiday-fixed 3 8 "妇女节")
       (holiday-fixed 3 9 " 妞妞生日")
       (holiday-fixed 4 1 "愚人节")
       (holiday-fixed 5 1 "劳动节")
       (holiday-fixed 2 5 "元宵节")
       (holiday-fixed 4 4 "清明节")
       (holiday-fixed 6 22 "端午节")
       (holiday-fixed 9 3 "我的生日")
       (holiday-fixed 9 28 "中秋节")
       (holiday-fixed 10 10 "老婆生日")
       (holiday-float 5 0 2 "母亲节")
       (holiday-float 6 0 3 "父亲节")
       (holiday-fixed 10 1 "国庆节")
       (holiday-fixed 12 25 "圣诞节")))

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
    plantuml-jar-path "~/.spacemacs.d/layers/pelm-org/vendor/plantuml.jar"
    puml-plantuml-jar-path "~/.spacemacs.d/layers/pelm-org/vendor/plantuml.jar"
    org-plantuml-jar-path "~/.spacemacs.d/layers/pelm-org/vendor/plantuml.jar"
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
  (setq org-confirm-babel-evaluate nil)
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
  (setq org-gcal-file-alist '(("eggcaker@gmail.com" . "~/.org-files/google.org")))
  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                    (org-agenda-files :maxlevel . 9))))
  (setq cfw:org-capture-template
    '("c" "calfw2org" entry
       (file "~/.org-files/refile.org")
       "*  %?\n %(cfw:org-capture-day)"))
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
  ;;   :name "mandian"
  ;;   :default t
  ;;   :client-id pacer/slack-client-id
  ;;   :client-secret pacer/slack-client-secret
  ;;   :token pacer/slack-token
  ;;   :subscribed-channels '(ci general))

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

  ;; Load lab code
  (when (file-exists-p "~/Desktop/test.el")
    (load "~/Desktop/test.el"))

  ;; roswell 
  (setq inferior-lisp-program "ros -Q run")
  (load (expand-file-name "~/.roswell/helper.el"))
  (setf slime-lisp-implementations
    `((sbcl    ("sbcl" "--dynamic-space-size" "2000"))
       (roswell ("ros" "-Q" "run"))))
  (setf slime-default-lisp 'roswell)
  (setq inferior-lisp-program "ros -L sbcl -Q -l ~/.sbclrc run")


  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

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
    (yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode web-beautify volatile-highlights vmd-mode vi-tilde-fringe uuidgen use-package toc-org tagedit symon string-inflection sql-indent spaceline smex smeargle slim-mode shell-pop scss-mode sass-mode reveal-in-osx-finder restart-emacs rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode plantuml-mode pip-requirements persp-mode pcre2el pbcopy password-generator paradox ox-twbs ox-reveal ox-gfm osx-trash osx-dictionary orgit org-trello org-projectile org-present org-pomodoro org-journal org-download org-clock-convenience org-bullets org-brain open-junk-file ob-restclient ob-http neotree nameless multi-term move-text mmm-mode meghanada markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode launchctl keyfreq key-chord json-mode js2-refactor js-doc ivy-purpose ivy-hydra insert-shebang info+ indent-guide impatient-mode ibuffer-projectile hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make groovy-mode gradle-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy flycheck-pos-tip flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-search-highlight-persist evil-org evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-embrace evil-ediff evil-commentary evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help ensime emms emmet-mode elisp-slime-nav elfeed-web elfeed-org elfeed-goodies editorconfig dumb-jump dracula-theme doom-themes diff-hl cython-mode csv-mode counsel-projectile counsel-osx-app counsel-ebdb company-web company-tern company-statistics company-shell company-restclient company-quickhelp company-emacs-eclim company-ebdb company-anaconda column-enforce-mode coffee-mode clean-aindent-mode buttercup browse-at-remote auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
