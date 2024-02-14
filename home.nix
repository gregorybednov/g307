{ config, pkgs, stdenv, ... }:
let
  #	server = "192.168.0.20:30000";
  #	nixgl = import <nixgl> {};
	astra = ! (builtins.isNull (builtins.match ".*astra.*" (builtins.readFile (pkgs.runCommandNoCC "hello" {} "uname -a > $out"))));
in {
  imports = [ ./mysqld.nix ./desktop.nix ];
  home.username = "student";
  home.homeDirectory = "/home/${config.home.username}";
  home.stateVersion = "23.11";
  
  gtk = {
#    enable = ! astra;
    theme = {
      name = "orchis-theme";
      package = pkgs.orchis-theme;
    };
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
    };
    cursorTheme = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
    };
  };

  fonts.fontconfig.enable = true;
  programs.vscode = {
    #enable = true;
    enableUpdateCheck = false;
    package = pkgs.vscode-fhs;
    # extensions = []; # TODO узнать кому что надобно
  };

  programs.firefox = {
	enable = true;
  };

  programs.yt-dlp = {
    enable = true;
    package = pkgs.yt-dlp-light;
  };

  programs.pylint.enable = true;

  home.packages = with pkgs; [
    emacs29-gtk3
    pinta
    unrar
    git
    sakura
    unzipNLS
    mate.engrampa
    liberation_ttf
    libreoffice
    mysql80
    dbeaver
    staruml
    archi
    plantuml
    jetbrains.pycharm-community
    jetbrains.idea-community
    jdk
    kotlin
    stm32cubemx
    nodePackages.node-red

    # POSIX utils capability
    at
    om4
    pax
    mailutils
    sharutils
    flex
    yacc
    universal-ctags
    inetutils
    uucp
    util-linux
    cflow
    ncompress
    clang

    # Nice programming language checkers and helpers
    gdb
    shellcheck
    valgrind
    cpplint
    cppcheck
    nixfmt
    golint
    errcheck
    go-tools
    eslint_d
    #flake8
    html-tidy
    
    # Basic network utils for education and debugging
    socat
    netcat

    # M$ fonts and for M$ VS Code
    corefonts
    (pkgs.nerdfonts.override { fonts = [ "DroidSansMono" ]; })
    jetbrains-mono
    emacs-all-the-icons-fonts
  ];

  home.file.".emacs.d/init.el".text = ''
;;; package --- Configuration
;;; Commentary:
;;; Code:

;(server-start)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package evil :straight t
  :disabled
  :custom
  (evil-toggle-key "C-[")
  :init
  (evil-mode 1))

(use-package jetbrains-darcula-theme :straight t
  :config
  (set-frame-font "JetBrains Mono 16")
  :init
  (load-theme 'jetbrains-darcula t))

(use-package all-the-icons ; TODO TODO TODO OOOO
  :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el")
  )

(use-package centaur-tabs :straight t
  :custom
  (centaur-tabs-style "bar")
  (centaur-tabs-set-icons (if (display-graphic-p) t nil))
  (centaur-tabs-set-bar 'over)
  :init
  (centaur-tabs-mode t)
  (centaur-tabs-enable-buffer-reordering)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package wakib-keys :straight t
  :init
  (wakib-keys 1))

(use-package json-mode :straight t :mode "\\.json\\'")
(use-package nix-mode :straight t :mode "\\.nix\\'")
(use-package go-mode :straight t :mode "\\.go\\'")

(use-package shellcop :straight t
  :hook ((sh-mode . shellcop-start)))

(use-package lsp-mode :straight t
  :custom
  (python-indent-offset 4)
  (lsp-sqls-workspace-config-path nil)
  (lsp-sqls-connections
    '(
      ((driver . "mysql") (dataSourceName . "root:root@tcp(127.0.0.1:3306)"))
      )
    )
  (lsp-keymap-prefix "C-l")
  :hook (
         (c-mode . lsp-mode)
         (c++-mode . lsp-mode)
         (python-mode . lsp-mode)
         (nix-mode . lsp-mode)
	 (go-mode . lsp-mode)
	 (sql-mode . lsp-mode)
	 (sql-mode . (lambda () (sql-set-product "mysql")))
	 (java-mode . lsp-mode)
	 (json-mode . lsp-mode))
  :commands lsp)

(use-package lsp-ui :straight t
  :custom
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-show-with-mouse t)
  :commands lsp-ui-mode)

(use-package company :straight t
  :custom
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3)))
  (company-text-icons-add-background t)
  :hook
  (
   (lsp-mode . company-mode)
   (sh-mode . company-mode)))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package flycheck :straight t
  :hook
  (
  (sh-mode . flycheck-mode)
  (emacs-lisp-mode . flycheck-mode)
  (lsp-mode . flycheck-mode)
  (nix-mode . flycheck-mode)
  (go-mode . flycheck-mode)
  (web-mode . flycheck-mode)
  (python-mode . flycheck-mode)
  (json-mode . flycheck-mode)
  (c++-mode . flycheck-mode)))

(use-package lsp-java :straight t
  :hook
  ((java-mode . lsp))
  )





;(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
;(flycheck-define-checker plantuml
;  "A checker using plantuml. See `http://plantuml.com"
;  :command ("plantuml" "-syntax")
;  :standard-input t
;  :error-patterns ((error line-start "ERROR" "\n" line "\n" (message) line-end))
;  :modes plantuml-mode)

;(defun flycheck-plantuml-setup ()
;  "Setup flycheck-plantuml.

;Add `plantuml' to `flycheck-checkers'."
;  (interactive)
;  (add-to-list 'flycheck-checkers 'plantuml))

;(setq plantuml-default-exec-mode 'executable)
;(setq plantuml-executable-args (list "-headless" "-theme" "spacelab"))

;(add-hook 'plantuml-mode-hook
;	  (lambda ()
;	    (flycheck-mode)
;	    (flycheck-plantuml-setup)
;	    ))



(provide 'init)
;;; init.el ends here
'';

  home.file.".emacs.d/early-init.el".text = ''
(setq inhibit-startup-message t)
(setq package-enable-at-startup nil)
  '';

  home.file.".config/shepherd/init.scm".text = ''
;; init.scm -- default shepherd configuration file.

(define (make-service . args)
  (apply make <service> args))

;; Services known to shepherd:
;; Add new services (defined using 'make <service>') to shepherd here by
;; providing them as arguments to 'register-services'.
(register-services
  (make-service
    #:docstring "MCron"
    #:provides '(mcron)
    #:start (make-forkexec-constructor '("${pkgs.mcron}/bin/mcron"))
    #:stop (make-kill-destructor)
    #:respawn? #t)
  (make-service
   #:docstring "Dropbear user SSHD"
   #:provides '(dropbear)
   #:start (make-forkexec-constructor '("${pkgs.dropbear}/bin/dropbear" "-E" "-F" "-r" "${config.home.homeDirectory}/.config/dropbear/dropbear_rsa_host_key" "-p" "2222"))
   #:stop (make-kill-destructor)
   #:respawn? #t)
  (make-service
    #:docstring "Emacs daemon"
    #:provides '(emacs)
    #:start (make-forkexec-constructor '("${pkgs.emacs29-gtk3}/bin/emacs" "--fg-daemon"))
    #:stop (make-kill-destructor)
    #:respawn? #t))


;; Send shepherd into the background
(action 'shepherd 'daemonize)

;; Services to start when shepherd starts:
;; Add the name of each service that should be started to the list
;; below passed to 'for-each'.
(for-each start '(emacs dropbear))  
'';

  home.file.".ssh/authorized_keys".text = ''
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCyBYo/E/FkFZVABzMixLS2TWaipfN5T24y8f+E6Px1t+IG8PLnQ38dLJiCR8k971DOycLuJUfKWsC06BK3XLWTO0+PmpfGKNT4NI6dwP2REl/umaignP/QQSs2w9Ff49WqPjIYTSmATTsCNZSVB0VtM0eJ+Y9Ff4CXb1frtt4GYztk6XB3jc3TxV72qzB0g6DqrHkf6pT5YAq2UeuFGZYSZCqBvVXCGcvKHkO1KBubuo95itVA5XbzK3INQTZpQowbtK4ULhUYlaGBcX5tYq1bdiTCDlTcLt6MfxYfFHSFiHbJOzdGPd+mXM7urOQhq49uQOf07dHt9qAzQHajItQb+X3FOgyFt4n6Y9Q37gn/6KC3PH1zClldq9DtgttuG/Xk15q+uvCldji9YIgb80aRHBIp6DY8PlYodmGGesBLiBMGQ7hgKupfkqszjfMkxnMYIkZodUCQVgaqsxsEQ7lD84JJMgBY8HkNLxKhy+6dP6kTc4vTtrpjezq/Ph95PFE= bednov@kafpi-108-1-mainserver2''; # stupid way but it works!!!!
  home.file.".profile".enable = ! astra;
  home.file.".profile".text = ''
    if [ -n "$BASH_VERSION" ]; then
        # include .bashrc if it exists
        if [ -f "$HOME/.bashrc" ]; then
    	. "$HOME/.bashrc"
        fi
    fi

    if [ -d "$HOME/bin" ] ; then
        PATH="$HOME/bin:$PATH"
    fi

    if [ -d "$HOME/.local/bin" ] ; then
        PATH="$HOME/.local/bin:$PATH"
    fi

    #export PATH="$PATH:/home/student/.local/share/JetBrains/Toolbox/scripts"


    if [ -e /home/student/.nix-profile/etc/profile.d/nix.sh ]; then . /home/student/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

    if [ -e /home/student/.local/bin/nix-user-chroot ]; then
            if [ -z "$NIX_CONF_DIR" ]; then
                    nix-user-chroot ~/.nix bash -l
            fi
    fi

    . "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"
    export XDG_DATA_DIRS="$HOME/.nix-profile/share:$XDG_DATA_DIRS"
    export LOCALE_ARCHIVE="/usr/lib/locale/locale-archive";

if [ ! -e $HOME/.config/dropbear ]
then
    mkdir -p $HOME/.config/dropbear
fi

if [ ! -e $HOME/.config/dropbear/dropbear_rsa_host_key ]
then
${pkgs.dropbear}/bin/dropbearkey -t rsa -f $HOME/.config/dropbear/dropbear_rsa_host_key
fi

${pkgs.dropbear}/bin/dropbear -r "$HOME/.config/dropbear/dropbear_rsa_host_key" -P "$HOME/.config/dropbear/dropbear.pid" -p 2222 -B -E 2>/dev/null &
    
    if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
    startx
    fi
    	'';

  
  
  home.sessionVariables.LOCALE_ARCHIVE =
    "${pkgs.glibcLocales}/lib/locale/locale-archive";
  programs.home-manager.enable = true;
}

