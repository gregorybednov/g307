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

(use-package evil
  :disabled
  :custom
  (evil-toggle-key "C-[")
  :straight t
  :init
  (evil-mode))

(use-package jetbrains-darcula-theme
  :straight t
  :config
  (set-frame-font "JetBrains Mono 14")
  :init
  (load-theme 'jetbrains-darcula t))

(use-package all-the-icons ; TODO TODO TODO OOOO
  :if (display-graphic-p)
  :straight (all-the-icons :type git :host github :repo "domtronn/all-the-icons.el")
  )

;(use-package all-the-icons
;  :if (display-graphic-p)
;  :straight t
;  :config
;  (all-the-icons-install-fonts t)
;  )

(use-package centaur-tabs
  :straight t
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

(use-package wakib-keys
  :straight t
  :init
  (wakib-keys 1))

(use-package json-mode :straight t :mode "\\.json\\'")
(use-package nix-mode :straight t :mode "\\.nix\\'")

(use-package shellcop
  :hook
  ((sh-mode . shellcop-start))
  :straight t)

(use-package lsp-mode
  :after company-lsp 
  :straight t
  :custom
  (lsp-keymap-prefix "C-l")
  :hook (
         (c-mode . lsp)
         (c++--mode . lsp)
         (python-mode . lsp)
         (nix-mode . lsp)
	 (json-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package company
  :straight t
  :custom
  (company-idle-delay (lambda () (if (company-in-string-or-comment) nil 0.3)))
  (company-text-icons-add-background t)
  :hook (prog-mode . company-mode))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :straight t
  :hook
  (
  (sh-mode . flycheck-mode)
  (emacs-lisp-mode . flycheck-mode)
  (c-mode . flycheck-mode)
  (nix-mode . flycheck-mode)
  (web-mode . flycheck-mode)
  (python-mode . flycheck-mode)
  (json-mode . flycheck-mode)
  (c++-mode . flycheck-mode)))

;(defun run-python ()
;  (interactive)
;  (quickrun))

;(setq python-shell-interpreter "python3")
;(elpy-enable)

;(add-hook 'python-mode-hook (lambda () (elpy-enable)))
;(add-hook 'python-mode-hook (lambda () (outline-minor-mode)))
;(add-hook 'python-mode-hook
;	  (lambda ()
;	    (tool-bar-add-item "spell" 'run-python
;			       'run-python
;			       :help   "Run python script")))








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
