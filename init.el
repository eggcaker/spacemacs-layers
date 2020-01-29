;; -*- mode: emacs-lisp; lexical-binding: t -*-
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
   '(rust
     octave
     ruby
     lua
     react
     html
     perl6
     ;; vimscript
     clojure
     ;; react
     bm
     emoji
     csv
     treemacs
     ;; html
     (chinese :variables
              chinese-enable-avy-pinyin nil
              chinese-enable-youdao-dict t)
     (dash :variables
           helm-dash-docset-newpath "~/Library/Application Support/Dash/DocSets")
     ;;csv
     ;;selectric
     ;;octave
     ;;nginx
     ;;octave
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
     (ivy :variables
          ivy-extra-directories nil)

     (javascript :variables
                 js2-basic-offset 2
                 js-indent-level 2
                 node-add-modules-path t
                 javascript-repl 'node
                 javascript-backend 'lsp)
     (erc :variables
          erc-server-list
          '(
            ("irc.gitter.im" "#syl20bnr/spacemacs" )
            ("irc.gitter.im" "#magit/magit" )
            ("freenode\\.net" "#org-mode")
            ))
     ;;vinegar
     ;; twitter
     emacs-lisp
     ;;common-lisp
     plantuml
     ;; (latex :variables
     ;;        latex-build-command "LaTex"
     ;;        latex-enable-folding t
     ;;        latex-enable-magic t
     ;;        latex-enable-auto-fill t)
     (org :variables
          org-enable-trello-support t
          org-want-todo-bindings t
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
     (shell :variables
            shell-default-shell 'term)
     ;;shell-scripts
     syntax-checking
     version-control
     ;;jabber
     ;;restructuredtext
     ;;docker
     ;; (go :variables
     ;;     go-tab-width 2
     ;;     gofmt-command "goimports"
     ;;     go-use-gometalinter t)
     ;; java
     ;; julia
     kotlin
     (dart :variables
           dart-sdk-path "~/src/tools/flutter/bin/cache/dart-sdk/"
           dart-enable-analysis-server t
           dart-format-on-save t)

     ;;vimscript
     ;;nginx
     ;;lua
     ;;(typescript :variables
     ;; typescript-fmt-on-save t)
     ;; csv
     osx
     ;; swift
     ;; ipython-notebook
     ;;pelm-elpy
     lsp
     dap
     (python :variables
             python-backend  'lsp
             python-fill-column 110
             python-sort-imports-on-save t
             python-enable-yapf-format-on-save t)
     evil-commentary
     ;; finance
     ;; (elfeed :variables
     ;;         url-queue-timeout 30
     ;;         elfeed-enable-web-interface nil
     ;;         rmh-elfeed-org-files (list "~/.spacemacs.d/pelm-feed/feeds.org"))

     ;; restclient
     (restclient :variables
                 restclient-use-org t)

     ;; (mu4e :variables
     ;;       mu4e-account-alist t
     ;;       mu4e-installation-path "/usr/local/Cellar/mu/HEAD/share/emacs/site-lisp/mu/mu4e")
     imenu-list
     ;;; just for fun
     ;; games
     ;; xkcd
     ;; gnus
     search-engine
     ;; Personal Layers
     twitter
     pelm-misc
     pelm-org
     ;; pelm-org-trello
     ;; pelm-contact
     pelm-xonsh
     ;;pelm-music
     ;;pelm-blog
     ;;pelm-finance
     ;;google-calendar
     ;;spotify
     ;;slack
     ;;pelm-ibuffer
     ;; pelm-erc
     ;; pelm-mail
     ;;pelm-slack
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(key-chord  pamparam prettier-js all-the-icons ox-reveal nameless groovy-mode keyfreq org-clock-convenience buttercup editorconfig evil-embrace counsel-osx-app)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(pangu-spacing exec-path-from-shell )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
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

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
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
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
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

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.

   dotspacemacs-default-font '("Fira Code"
                               :size 18
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
   dotspacemacs-remap-Y-to-y$ t
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

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

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

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
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

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

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
   dotspacemacs-frame-title-format "%F%I@%S"

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
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  (setq configuration-layer-elpa-archives
        '(("melpa-cn" . "http://mirrors.cloud.tencent.com/elpa/melpa/")
          ("org-cn"   . "http://mirrors.cloud.tencent.com/elpa/org/")
          ("gnu-cn"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")))

  ;; Set the Emacs customization file path. Must be done here in user-init.
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-title-bar nil)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (setq custom-file "~/.spacemacs.d/custom.el")
  (setq org-contacts-files   '("~/.org-files/contacts/contacts.org"))
  (setq standard-indent 2)
  (setenv "LANG" "en_US.UTF-8")
  (setenv "PATH" "/usr/local/bin:/usr/bin:~/.dotfiles/bin:/bin")
  (setq exec-path '("/usr/local/bin" "~/.dotfiles/bin/" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/Cellar/emacs-plus/26.1/libexec/emacs/26.1/x86_64-apple-darwin17.4.0"))
  (setq-default
   ;; remove the 4m from shell
   system-uses-terminfo nil
   exec-path-from-shell-check-startup-files nil
   google-translate-default-target-language "zh"

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
   sp-escape-wrapped-region nil
   sp-escape-quotes-after-insert nil

   ;; Magit
   magit-popup-show-common-commands nil
   git-commit-summary-max-length 60
   git-magit-status-fullscreen t
   magit-refresh-status-buffer t
   magit-commit-show-diff nil
   magit-revert-buffers t

   magit-repository-directories '(
                                  ("~/.spacemacs.d/" . 0)
                                  ("~/.emacs.d/" . 0)
                                  ("~/src/work/pacer_android/" . 0)
                                  ("~/.dotfiles" . 0))

   ;; Flycheck
   avy-all-windows 'all-frames
   flycheck-check-syntax-automatically '(save mode-enabled)

   ;; Avy
   avy-all-windows 'all-frames

   ;; Spaceline
   ;;spaceline-buffer-encoding-abbrev-p nil
   ;;spaceline-version-control-p nil

   ;; flycheck
   flycheck-jshintrc "~/.jshintrc"
   flycheck-jscsrc "~/.jscsrc"
   flycheck-eslintrc "~/.eslintrc"
   ;; Theme modifications
   theming-modifications '()
   ))

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  (setq explicit-shell-file-name "/usr/local/bin/zsh")
  (setq shell-file-name "/usr/local/bin/zsh")

  (setq engine/browser-function 'eww-browse-url)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-strict-missing-semi-warning nil)
  (setq twittering-display-remaining t)
  (setq twittering-use-master-password t)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-file-suffixes
        '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar" "*.class"))
  (setq projectile-globally-ignored-directories
        '(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))

  (setq calendar-week-start-day 1)
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq python-indent-guess-indent-oftpfset nil)
  (evil-declare-change-repeat 'company-complete)
  (setq counsel-osx-app-location '("/Applications" "/Applications/Utilities"))
  (setq python-shell-interpreter "python")
  (setq python-shell-interpreter-args "-i")
  (setq python-shell-exec-path '("/usr/local/bin/"))
  ;; (setq ob-ipython-command "/usr/local/bin/jupyter")
  (setq dotspacemacs-scratch-mode 'org-mode)
  (setq calc-settings-file "~/.emacs.d/.cache/calc.el")
  (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)
  (spacemacs/set-leader-keys "op" 'youdao-dictionary-play-voice-at-point)
  (setq python-indent-offset 4)
  (setq counsel-git-cmd "rg --files")

  (setq counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")

  (setq gnus-secondary-select-methods
        '(
          (nntp "gmane"
                (nntp-address "news.gmane.org"))
          (nntp "news.eternal-september.org")
          (nntp "nntp.aioe.org")
          (nntp "news.gwene.org")
          (nnimap "gmail"
                  (nnimap-address
                   "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl))))

  (setq user-full-name "Tongzhu, Zhang")
  (setq user-mail-address "eggcaker@gmail.com")
  ;; gmail setup
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587)
  (setq smtpmail-smtp-user "eggcaker@gmail.com")
  
  (setq send-mail-function 'smtpmail-send-it) ; not for Gnus
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-stream-type 'starttls
        starttls-use-gnutls t) ; for Gnus
  (setq smtpmail-default-smtp-server "smtp.gmail.com")
  (setq smtpmail-local-domain "smtp.gmail.com")
  (setq smtpmail-sendto-domain "smtp.gmail.com")
  (setq smtpmail-debug-info t) ; only to debug problems

  ;; Archive outgoing email in Sent folder on imap.gmail.com:
  (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmail]/Sent Mail")

  (setq gnus-posting-styles
        '(((header "to" "eggcaker@gmail.com")
           (address "eggcaker@gmail.com"))
          ((header "to" "eggcaker@gmail.com")
           (address "address@gmail.com"))))

  ;; store email in ~/gmail directory
  (setq nnml-directory "~/.gmail")
  (setq message-directory "~/.gmail")



  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
  (setq org-agenda-show-future-repeats nil)

  ;; Load local.el first
  (when (file-exists-p "~/.local.el")
    (load "~/.local.el"))

  (global-git-commit-mode t)

  ;;Fira Code
  (when (spacemacs/system-is-mac)
    (spacemacs//set-monospaced-font "JetBrains Mono" "Hiragino Sans GB" 18  22))


  (when (spacemacs/system-is-linux)
    (spacemacs//set-monospaced-font "Source Code Pro" "Droid Sans Fallback" 18 20))

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
   org-plantuml-jar-path "~/.spacemacs.d/layers/pelm-org/vendor/plantuml.jar")

  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'calculator-mode 'emacs)
  (evil-define-key 'emacs term-raw-map (kbd "C-c") 'term-send-raw)
  (setq org-confirm-babel-evaluate nil)
  (global-company-mode)
  (turn-off-show-smartparens-mode)

  ;; groovy for gradle file
  (add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

  (setq display-time-mode t)
  (setq-default line-spacing 10)
  (setq org-bullets-bullet-list '("☯" "☰" "☵" "☶" "☳" "☴" "☲" "☷" "☱" ))
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
  (evil-leader/set-key "aa" 'counsel-osx-app)

  ;; Load lab code
  (when (file-exists-p "~/Desktop/test.el")
    (load "~/Desktop/test.el"))

  ;; temp add hydra for rectagle here
(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :hint nil
                                     :post (deactivate-mark))
  "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
  ("k" rectangle-previous-line)
  ("j" rectangle-next-line)
  ("h" rectangle-backward-char)
  ("l" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("g" nil))      ;; ok

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
   '(org-trello-current-prefix-keybinding "C-c o")
   '(package-selected-packages
     (quote
      (org-trello request-deferred emms counsel-ebdb company-ebdb ebdb youdao-dictionary yasnippet-snippets yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-beautify volatile-highlights vmd-mode vi-tilde-fringe uuidgen use-package treemacs-projectile treemacs-evil toc-org symon string-inflection sql-indent spaceline-all-the-icons smex smeargle shell-pop reveal-in-osx-finder restart-emacs request rainbow-delimiters pyvenv pytest pyim pyenv-mode py-isort popwin plantuml-mode pippel pip-requirements persp-mode pcre2el pbcopy password-generator paradox pangu-spacing ox-twbs ox-reveal ox-gfm overseer osx-trash osx-dictionary orgit org-projectile org-present org-pomodoro org-mime org-journal org-download org-clock-convenience org-bullets org-brain open-junk-file ob-restclient ob-ipython ob-http neotree nameless mvn multi-term move-text mmm-mode meghanada maven-test-mode markdown-toc magit-gitflow macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint ledger-mode launchctl kotlin-mode keyfreq key-chord json-mode js2-refactor js-doc ivy-purpose ivy-hydra insert-shebang info+ indent-guide importmagic ibuffer-projectile hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-purpose helm-projectile helm-mode-manager helm-make helm-flx helm-descbinds helm-ag groovy-mode groovy-imports gradle-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy flycheck-pos-tip flycheck-ledger flycheck-kotlin flycheck-bashate flx-ido fish-mode find-by-pinyin-dired fill-column-indicator fancy-battery eyebrowse exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-embrace evil-ediff evil-commentary evil-cleverparens evil-args evil-anzu eval-sexp-fu ess-R-data-view eshell-z eshell-prompt-extras esh-help ensime elisp-slime-nav elfeed-org editorconfig dumb-jump diminish diff-hl define-word dash-at-point cython-mode csv-mode counsel-projectile counsel-osx-app counsel-dash company-tern company-statistics company-shell company-restclient company-quickhelp company-emacs-eclim company-anaconda column-enforce-mode coffee-mode clean-aindent-mode buttercup browse-at-remote auto-yasnippet auto-highlight-symbol auto-compile aggressive-indent adaptive-wrap ace-pinyin ace-link ace-jump-helm-line ac-ispell)))
   '(tramp-syntax (quote default) nil (tramp)))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   )
  )
