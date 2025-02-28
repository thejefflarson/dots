;; jeffs-init --- various bits and boops to make emacs a nicer place

;;; Commentary:
;; Too many late nights copying and pasting from the internet,
;; but hey I'm proud of it.
;;; Code:

;; Configuration
(setq gc-cons-threshold (eval-when-compile (* 100 1024 1024)))

(setq-default indent-tabs-mode nil
            tab-width 2
            fill-column 100)

(setq user-full-name "Jeff Larson"
     user-mail-address "thejefflarson@gmail.com"
     load-prefer-newer t
     cursor-in-non-selected-windows nil
     highlight-nonselected-windows nil
     column-number-mode t)

(tooltip-mode -1)
;; Allow the -l flag to find the right emacs directory
(defconst user-emacs-directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Current emacs.d directory.")

(defconst user-cache-directory
  (file-name-as-directory
   (concat user-emacs-directory ".cache"))
  "Directory for temporary files.")

(defconst user-lisp-directory
  (file-name-as-directory
   (concat user-emacs-directory "lisp"))
  "Directory for extra Lisp files.")
(add-to-list 'load-path user-lisp-directory)

(defun ensure-directory (dir)
  "Make directory DIR if it doesn't exist."
  (unless (and (file-exists-p dir)
               (file-accessible-directory-p dir))
    (mkdir dir)))

(ensure-directory user-cache-directory)

;; OS X specific configuration
(when (eq system-type 'darwin)
  (setenv "SHELL" "/usr/local/bin/fish")
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq-default mac-pass-command-to-system nil)
  (setq-default mac-emulate-three-button-mouse t)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)
  (setq-default epg-gpg-program  "/usr/local/bin/gpg")
  (setq mouse-wheel-scroll-amount '(0.01))
  (if (file-exists-p  "/usr/local/share/emacs/site-lisp/")
      (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
        (normal-top-level-add-subdirs-to-load-path))))

;; Linux specific configuration
(when (equal system-type 'gnu/linux)
  (let ((default-directory "/usr/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

;; Don't muddy init.el with custom stuff
(setq-default custom-file
              (concat user-cache-directory "custom.el"))
(if (file-exists-p custom-file)
    (load-file custom-file)
  (write-region "" nil custom-file t))

;; put versions in the cache directory
(setq backup-by-copying t)
(setq backup-directory-alist `((".*" . ,user-cache-directory))
      backup-by-copying t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq-default delete-old-versions t)
(setq auto-save-file-name-transforms `((".*" ,user-cache-directory t)))
(setq create-lockfiles nil)

;; Turn off a bunch of useless stuff
(when (featurep 'menu-bar)
  (menu-bar-mode -1))
(when (featurep 'tool-bar)
  (tool-bar-mode -1))
(when (featurep 'scroll-bar)
  (scroll-bar-mode -1))
(setq inhibit-startup-screen t)
(setq ring-bell-function #'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Only utf-8 please
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; Revert when file changes
(global-auto-revert-mode t)
(setq-default imenu-auto-rescan t)

;; text-mode should wrap
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; nicer resizing
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)

;; Use gnu ls on darwin
(when (eq system-type 'darwin)
  (setq-default dired-use-ls-dired t)
  (setq insert-directory-program "/opt/homebrew/bin/gls")
  (setq dired-listing-switches "-aBhl --group-directories-first"))

;; other niceties
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(show-paren-mode 1)
(electric-pair-mode 1)
;; make electric pair mode not do quotes
(setq-default electric-pair-inhibit-predicate
              (lambda (c)
                (if (char-equal c ?\") t (electric-pair-default-inhibit c))))
(recentf-mode)
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region)
(delete-selection-mode 1)
(windmove-default-keybindings 'super)
(setq-default abbrev-mode -1)
(setq-default doc-view-resolution 300)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq sentence-end-double-space nil)

;; Blinky hairline cursor
(setq-default cursor-type '(bar . 2))
(blink-cursor-mode 1)
(setq-default tab-always-indent 'complete)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)


;; Server code
(unless (string-equal "root" (getenv "USER"))
  (require 'server)
  (unless (server-running-p) (server-start)))


;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(diminish 'auto-revert-mode)
(setq use-package-always-ensure t)
(setq-default use-package-verbose t)
;;(use-package use-package-ensure-system-package
;;  :ensure t)
(use-package buffer-move)
(use-package gnu-elpa-keyring-update)
(use-package cl-lib)

;; Theme
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable current color scheme, before enabling another."
  (mapc #'disable-theme custom-enabled-themes))
(ad-activate 'load-theme)

(require 'elegance)

(let (light true)
  (defun heaven-and-hell ()
    (interactive)
    (if light (elegance-light) (elegance-dark))
    (setq light (not light))))
(bind-key "<f6>" 'heaven-and-hell)
(elegance-light)

(setq-default org-fontify-whole-heading-line t
              org-fontify-done-headline t
              org-fontify-quote-and-verse-blocks t)

(use-package unicode-fonts
   :ensure t
   :config
   (unicode-fonts-setup))

;; guess indent
(use-package dtrt-indent
  :config
  (setq dtrt-indent-global-mode t))

;; More Packages
(require 'epa-file)
(epa-file-enable)

(use-package vlf
  :requires 'vlf-setup
  :custom
  (vlf-application 'dont-ask))

(use-package alert
  :custom
  (alert-default-style 'osx-notifier))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package crux
  :bind
  (("C-c o" . crux-open-with)
   ("C-c m" . crux-cleanup-buffer-or-region)
   ("C-c f" . crux-recentf-find-file)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c r" . crux-rename-file-and-buffer)
   ("C-c I" . crux-find-user-init-file)
   ("C-c i" . crux-ispell-word-then-abbrev))
  :config
  (crux-reopen-as-root-mode))

(use-package rainbow-delimiters
  :defer t
  :diminish rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy
  :commands ivy-mode
  :diminish ivy-mode
  :bind
  (("C-x b" . ivy-switch-buffer)
   ("C-x C-b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)
   ("C-c C-r" . ivy-resume))
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-selectable-prompt t)
  :config
  (ivy-mode 1))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package counsel
  :diminish counsel-mode
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-c f" . counsel-describe-function)
   ("C-c v" . counsel-describe-variable)
   ("C-c l" . counsel-load-library)
   ("C-c i" . counsel-info-lookup-symbol)
   ("C-c u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate))
  :config
  (counsel-mode))

(use-package swiper
  :bind
  (("C-s" . swiper)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

(setq-default flycheck-verilog-verilator-executable "verilator")
(use-package flycheck
  :commands flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-disabled-checkers
   '(javascript-jshint python-flake8 rust)))

(use-package ag
  :defer t)

(use-package ripgrep
  :defer t)

(use-package projectile-ripgrep
   :defer t)

(use-package treemacs
  :after treemacs-all-the-icons
  :config
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'extended)
  (treemacs-load-theme "all-the-icons"))
(use-package all-the-icons)
(use-package treemacs-all-the-icons)

(use-package treemacs-projectile
  :after treemacs projectile
  :custom
  (treemacs-header-function #'treemacs-projectile-create-header))

(use-package treemacs-magit
  :after treemacs magit)

(use-package projectile
  :bind
  (("C-c p" . projectile-switch-project)
   ("s-p" . projectile-command-map))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  (projectile-switch-project-action #'treemacs-projectile)
  (treemacs-file-face 'default)
  :config
  (projectile-mode))

(use-package magit
  :bind (("C-c s" . magit-status))
  :custom
  (magit-completing-read-function 'ivy-completing-read))

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package kurecolor)

(use-package rainbow-mode
  :hook (css-mode web-mode typescript-mode emacs-list-mode))

(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode 1))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :config
  (global-page-break-lines-mode))

(use-package winner
  :config
  (winner-mode t))

(use-package eshell
  :defer t
  :custom
  (eshell-buffer-maximum-lines 20000)
  (eshell-history-size 350)
  (eshell-hist-ignoredups t)
  (eshell-plain-echo-behavior t)
  (eshell-directory-name
   (concat user-cache-directory "eshell/")))

(use-package shell-pop
  :bind (("C-t" . shell-pop))
  :custom
  (shell-pop-shell-type
        '("eshell" "*eshell*" (lambda () (eshell)))))

(use-package company
  :diminish company-mode
  :bind (("C-;" . company-indent-or-complete-common))
  :hook (prog-mode . company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1))

(use-package cmake-mode)

(use-package company-statistics
  :diminish t
  :custom
  (company-statistics-file
   (concat user-cache-directory
           "company-statistics-cache.el"))
  :hook (after-init . company-statistics-mode))

(use-package flyspell
  :hook
  ((flyspell-mode . flyspell-popup-auto-correct-mode)))

(use-package flyspell-popup
  :commands (flyspell-popup-auto-correct-mode))

(use-package deft
  :after org
  :bind
  ("C-x n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Documents/org/"))

(use-package org
  :commands org-agenda-list
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :hook
  (org . org-indent-mode)
  :init
  (ensure-directory "~/Documents/org/")
  (ensure-directory "~/Documents/org/roam")
  :custom
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)
  (org-log-refile 'time)
  (org-use-tag-inheritance t)
  (org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (org-mobile-inbox-for-pull "~/Documents/org/notes.org")
  (org-directory "~/Documents/org")
  (org-agenda-files (list "~/Documents/org/todos.org"))
  (org-default-notes-file "~/Documents/org/notes.org")
  (org-agenda-window-setup 'only-window)
  (org-hierarchical-todo-statistics nil)
  (org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/org/todos.org" "Tasks")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
          ("n" "Note" entry (file "~/Documents/org/notes.org")
           "* %?\nCaptured %<%Y-%m-%d %H:%M>")))
  (org-tag-alist '((:startgroup . nil)
                        ("@work" . ?w) ("@home" . ?h)
                        (:endgroup . nil)
                        ("phone" . ?p) ("meeting" . ?m)
                        ("code" . ?c) ("writing" . ?r)))
  (org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING(w@/!)" "|" "DONE(d@)" "CANCELED(c@)")))
  (org-enforce-todo-dependencies t)
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  :config
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-mobile))

(ensure-directory "~/Documents/org/roam")
(ensure-directory "~/Documents/org/roam/daily")
(setq-default org-roam-v2-ack t)

(use-package org-roam
  :diminish t
  :hook
  (after-init . org-roam-db-autosync-enable)
  :bind
  (("C-c b f" . org-roam-find-file)
   ("C-c b i" . org-roam-node-insert)
   ("C-c b d" . org-roam-dailies-goto-today))
  :diminish t
  :custom
  (org-roam-v2-ack t)
  (org-roam-complete-everywhere t)
  (org-roam-directory "~/Documents/org/roam")
  (org-roam-dailies-directory "~/Documents/org/roam/daily")
  (org-roam-db-autosync-mode)
  (org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :if-new (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n")))))

(use-package org-projectile
  :after org
  :bind
  (("C-c n p" . org-projectile-project-todo-completing-read))
  :custom
  (org-projectile-projects-file "~/Documents/org/todos.org"))

(use-package cider
  :defer t
  :bind
  ("C-c M-x" . cider-jack-in))

(use-package writeroom-mode
  :defer t)

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package auto-package-update
   :custom
   (auto-package-update-interval 4)
   (auto-package-delete-old-versions t)
   :config
   (auto-package-update-maybe))



;; smarts
(use-package ellama
    :bind ("C-c e" . ellama-transient-main-menu)
    :init
    ;; setup key bindings
    ;; (setopt ellama-keymap-prefix "C-c e")
    ;; language you want ellama to translate to
    (setopt ellama-language "English")
    ;; could be llm-openai for example
    (require 'llm-ollama)
    (setopt ellama-provider
	      (make-llm-ollama
	       ;; this model should be pulled to use it
	       ;; value should be the same as you print in terminal during pull
	       :chat-model "llama3.1:8b-instruct-q8_0"
	       :embedding-model "nomic-embed-text"
	       :default-chat-non-standard-params '(("num_ctx" . 8192))))
    (setopt ellama-summarization-provider
	      (make-llm-ollama
	       :chat-model "qwen2.5:3b"
	       :embedding-model "nomic-embed-text"
	       :default-chat-non-standard-params '(("num_ctx" . 32768))))
    (setopt ellama-coding-provider
	      (make-llm-ollama
	       :chat-model "qwen2.5-coder:3b"
	       :embedding-model "nomic-embed-text"
	       :default-chat-non-standard-params '(("num_ctx" . 32768))))
    ;; Predefined llm providers for interactive switching.
    ;; You shouldn't add ollama providers here - it can be selected interactively
    ;; without it. It is just example.
    (setopt ellama-providers
	      '(("zephyr" . (make-llm-ollama
			     :chat-model "zephyr:7b-beta-q6_K"
			     :embedding-model "zephyr:7b-beta-q6_K"))
		      ("mistral" . (make-llm-ollama
			      :chat-model "mistral:7b-instruct-v0.2-q6_K"
			      :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
		      ("mixtral" . (make-llm-ollama
			      :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
			      :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
    ;; Naming new sessions with llm
    (setopt ellama-naming-provider
	      (make-llm-ollama
	       :chat-model "llama3.1:8b-instruct-q8_0"
	       :embedding-model "nomic-embed-text"
	       :default-chat-non-standard-params '(("stop" . ["\n"]))))
    (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
    ;; Translation llm provider
    (setopt ellama-translation-provider
	    (make-llm-ollama
	     :chat-model "qwen2.5:3b"
	     :embedding-model "nomic-embed-text"
	     :default-chat-non-standard-params
	     '(("num_ctx" . 32768))))
    ;; customize display buffer behaviour
    ;; see ~(info "(elisp) Buffer Display Action Functions")~
    (setopt ellama-chat-display-action-function #'display-buffer-at-bottom)
    (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
    :config
    ;; send last message in chat buffer with C-c C-c
    (add-hook 'org-ctrl-c-ctrl-c-hook #'ellama-chat-send-last-message))


;; Programming modes

;; use treesit
(use-package treesit-auto
  :custom
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package chruby
  :custom
  (chruby "2.5.1"))

(use-package yaml-mode
  :mode "\\.yaml\\'")

(add-hook 'makefile-mode-hook
  (lambda () (setq indent-tabs-mode t)))

(use-package jinja2-mode
  :mode "\\.twig\\'")

(use-package projectile-rails
  :hook (ruby-mode . projectile-rails-on))

(use-package poetry
  :config (poetry-tracking-mode))

(use-package ruby-ts-mode
  :mode "\\.rb\\'"
  :init
  (add-to-list 'completion-ignored-extensions ".rbc")
  :custom
  (flycheck-rubocoprc nil))

(use-package python
  :mode "python-mode"
  :after (poetry)
  :hook
  (python-ts-mode .
     (lambda () (
        when (poetry-venv-exist-p)
          (setq-local lsp-pyls-server-command '("poetry" "run" "pylsp"))
        )
       (lsp-deferred)
     )))

(use-package pyvenv
  :hook (after-init . pyvenv-mode))

(setq-default c-basic-offset 2)
(setq-default c-syntactic-indentation nil)
(setq-default c-tab-always-indent t)
(setq-default c-toggle-auto-newline 1)

(use-package clang-format+
  :diminish clang-format+-mode
  :hook
  ((c-mode . clang-format+-mode)
   (c++-mode . clang-format+-mode)))

(use-package platformio-mode
  :hook (c++-mode . platformio-conditionally-enable))

;; dunno why this needs to be non async but ok
(defun colorize-compilation-buffer()
  "Color compilation."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(use-package ansi-color
  :hook (compilation-filter . colorize-compilation-buffer))

(use-package gdb-mi
  :commands gdb
  :defer t
  :custom
  (gdb-many-windows t)
  (gdb-show-main t))

(use-package css-mode
  :mode "\\.css\\'")

(use-package graphql-mode
  :mode "\\.graphql\\'")

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package clojure-mode
  :mode "\\.clj\\'")

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-max-width 50)
  (lsp-ui-doc-max-height 5)
  (lsp-diagnostic-package :flycheck)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-ui-sideline-show-code-actions t))

(use-package dap-mode
  :hook (lsp-mode)
  :custom
  (dap-auto-configure-mode))

(use-package dockerfile-ts-mode
  :mode "Dockerfile\\'")

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  ((rust-ts-mode . lsp-deferred)
   (verilog-mode . lsp-deferred)
   (python-ts-mode . lsp-deferred)
   (yaml-mode . lsp-deferred)
   (dockerfile-ts-mode . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "s-l")
  (lsp-prefer-flymake nil)
  (lsp-enable-file-watchers nil)
  (lsp-rust-server 'rust-analyzer)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (lsp-rust-enable-all-features t))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;(setq-default lsp-clients-verilog-executable "svls")
(use-package verilog-mode
  :ensure-system-package
  (svlangserver . "npm install -g @imc-trading/svlangserver")
   :hook
   (verilog-mode . (lambda ()
                     (setq flycheck-checker 'verilog-verilator)
                     (flycheck-add-next-checker 'verilog-verilator 'lsp)))
  :custom
  (verilog-indent-level 2)
  (verilog-case-indent-level 2)
  (verilog-indent-level-module 2)
  (verilog-indent-level-behavioral 2)
  (verilog-indent-level-declaration 2)
  (verilog-auto-newline nil)
  :config
  ;; Jesus autocomplete sucks in this mode, just because you can doesn't mean you should
  (clear-abbrev-table verilog-mode-abbrev-table))

(use-package rust-ts-mode
  :mode "\\.rs\\'"
  :ensure-system-package
  ((rls . "rustup component add rls rust-analysis rust-src")
  (rust-analyzer . "curl -L https://github.com/rust-analyzer/rust-analyzer/releases/download/2021-10-25/rust-analyzer-aarch64-apple-darwin.gz | gunzip  > ~/bin/rust-analyzer; chmod +x ~/bin/rust-analyzer"))
  :init
  (add-hook 'before-save-hook (lambda () (when (eq 'rust-ts-mode major-mode)
                                           (lsp-format-buffer)))))

(use-package typescript-ts-mode
  :mode (("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)))

(use-package rjsx-mode)
(use-package tide
  :ensure t
  :after (company flycheck)
  :hook ((typescript-ts-mode . tide-setup)
         (tsx-ts-mode . tide-setup)
         (typescript-ts-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)
(setq-default js2-basic-offset 2)
(setq-default js3-indent-level 2)
(setq-default typescript-indent-level 2)
(setq-default tide-format-options '(:indentSize 2 :tabSize 2))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package cargo
  :hook (rust-ts-mode . cargo-minor-mode))

(use-package fish-mode
  :mode "\\.fish\\'")

(use-package prettier-js
  :ensure-system-package (prettier . "npm i prettier -g")
  :hook ((js-mode json-mode css-mode scss-mode tide-mode) . prettier-js-mode))

(use-package json-mode
  :mode "\\.json\\'")

(use-package sql-indent
  :defer t)

(use-package sql
  :mode "\\.sql$"
  :after sql-indent chruby
  :hook sqlind-minor-mode
  :custom
  (sql-indent-offset 2))

(use-package lua-mode
  :mode "\\.lua$")

(use-package scss-mode
  :mode "\\.scss\\'"
  :custom
  (scss-compile-at-save nil))

(use-package markdown-mode
  :commands markdown-mode gfm-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown"))

(use-package terraform-mode
  :config
  (terraform-format-on-save-mode))

(use-package bison-mode
  :mode
  (("\\.l\\'" . bison-mode))
  (("\\.y\\'" . bison-mode)))

(use-package swift-mode
  :mode "\\.swift\\'")

(use-package go-mode
  :mode "\\.go\\'"
  :ensure-system-package
  (gopls . "go install golang.org/x/tools/gopls@latest")
  :hook
  ((before-save . gofmt-before-save)))

(use-package flycheck-swift
  :commands flycheck-swift-setup
  :hook (swift-mode . flycheck-swift-setup))

(defun no-auto-fill ()
  "Turn off 'auto-fill-mode'."
  (auto-fill-mode -1))

(message "Loaded in `%s'" (emacs-init-time))

(defun layout()
  "Create my custom layout."
  (interactive)
  (select-frame (make-frame '((user-position . t)
                              (width . 120)
                              (height . 40)
                              (top . -1)
                              (left . -1))))
  (org-agenda-list))

(provide 'init)
;;; init.el ends here
