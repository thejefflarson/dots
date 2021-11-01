
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
  "Directory for extra lisp files.")
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
  (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))

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
(setq backup-directory-alist `((".*" . ,user-cache-directory))
      backup-by-copying t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)
(setq-default delete-old-versions t)
(setq auto-save-file-name-transforms `((".*" ,user-cache-directory t)))

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

;; Use gnu ls on darwin
(when (eq system-type 'darwin)
  (setq-default dired-use-ls-dired t)
  (setq insert-directory-program "/usr/local/bin/gls")
  (setq dired-listing-switches "-aBhl --group-directories-first"))

;; other niceties
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(show-paren-mode 1)
(electric-pair-mode 1)
(recentf-mode)
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region)
(delete-selection-mode 1)
(windmove-default-keybindings 'super)
(setq-default abbrev-mode -1)
(setq-default doc-view-resolution 300)

;; Blinky hairline cursor
(setq-default cursor-type '(bar . 2))
(blink-cursor-mode 1)

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
(use-package use-package-ensure-system-package)
(use-package buffer-move)
(use-package gnu-elpa-keyring-update)
(use-package cl-lib)

;; Theme
(defadvice load-theme (before theme-dont-propagate activate)
  "Disable current color scheme, before enabling another."
  (mapc #'disable-theme custom-enabled-themes))
(ad-activate 'load-theme)

(require 'elegance)
(require 'cl-lib)
(lexical-let (light true)
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
  (global-undo-tree-mode))

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
  ((flyspell-mode . flyspell-popup-auto-correct-mode)
   (text-mode . flyspell-mode)))

(use-package flyspell-popup
  :commands (flyspell-popup-auto-correct-mode))

(use-package deft
  :after org
  :bind
  ("C-x C-g" . deft)
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
  (after-init . org-roam-setup)
  :bind
  (("C-c b f" . org-roam-find-file)
   ("C-c b i" . org-roam-node-insert)
   ("C-c b d" . org-roam-dailies-find-today))
  :diminish t
  :custom
  (org-roam-v2-ack t)
  (org-roam-complete-everywhere t)
  (org-roam-directory "~/Documents/org/roam")
  (org-roam-dailies-directory "~/Documents/org/roam/daily")
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

(use-package org-journal
  :bind
  ("C-c C-j" . org-journal-new-entry)
  :init
  (ensure-directory "~/Documents/journal/")
  :custom
  (org-journal-dir "~/Documents/journal/")
  (org-support-shift-select t))

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
   (auto-package-update-delete-old-versions t)
   (auto-package-update-interval 4)
   :config
   (auto-package-update-maybe))


;; Programming modes
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package chruby
  :custom
  (chruby "2.5.1"))

(use-package python
  :mode ("\\.py\\'" . python-mode))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :init
  (add-to-list 'completion-ignored-extensions ".rbc")
  :custom
  (flycheck-rubocoprc nil))

(use-package yaml-mode
  :mode "\\.yaml\\'")

(add-hook 'makefile-mode-hook
  (lambda () (setq indent-tabs-mode t)))

(use-package jinja2-mode
  :mode "\\.twig\\'")

(use-package projectile-rails
  :hook (ruby-mode . projectile-rails-on))

(use-package company-jedi
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package blacken
  :ensure-system-package (black . "pip install black")
  :hook (python-mode . blacken-mode))

(use-package poetry
  :ensure-system-package (poetry . "pip install poetry")
  :hook (python-mode . poetry-tracking-mode))

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
  (lsp-enable-on-type-formatting nil))

(use-package lsp-mode
  :commands lsp
  :hook
  ((prog-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-keymap-prefix "s-l")
  (lsp-prefer-flymake nil)
  (lsp-enable-file-watchers nil)
  (lsp-rust-server 'rust-analyzer)
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(setq-default lsp-clients-verilog-executable "svls")
(use-package verilog-mode
  :ensure-system-package
  (svls . "cargo install svls")
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

(use-package rust-mode
  :mode "\\.rs\\'"
  :ensure-system-package
  ((rls . "rustup component add rls rust-analysis rust-src")
  (rust-analyzer . "curl -L https://github.com/rust-analyzer/rust-analyzer/releases/download/2021-10-25/rust-analyzer-aarch64-apple-darwin.gz | gunzip  > ~/bin/rust-analyzer; chmod +x ~/bin/rust-analyzer"))
  :custom
  (rust-format-on-save t))

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package fish-mode
  :mode "\\.fish\\'")

(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)
(setq-default js2-basic-offset 2)
(setq-default js3-indent-level 2)
(setq-default typescript-indent-level 2)
(setq-default tide-format-options '(:indentSize 2 :tabSize 2))

(use-package web-mode
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.tsx\\'" . web-mode))
  :custom
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-current-element-highlight nil)
  (web-mode-enable-part-face t)
  (web-mode-enable-block-face t)
  (web-mode-enable-current-column-highlight t)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  :hook (web-mode . (lambda ()
                    (when (string-equal "tsx" (file-name-extension buffer-file-name))
                      (tide-setup)))))

(use-package js2-mode
  :mode
  (("\\.js\\'" . js2-mode))
  (("\\.jsx\\'" . js2-jsx-mode))
  :custom
  (js2-basic-offset 2)
  (js2-mode-show-parse-errors nil)
  (js2-mode-show-strict-warnings nil))

(use-package prettier-js
  :ensure-system-package (prettier . "npm i prettier -g")
  :hook ((js2-mode json-mode js2-jsx-mode css-mode scss-mode tide-mode) . prettier-js-mode))

(use-package json-mode
  :mode "\\.json\\'")

(use-package rjsx-mode)

(use-package tide
  :ensure-system-package
  ((tsc . "npm i typescript -g")
   (tslint . "npm i -g tslint")
   (tsfmt . "npm i -g typescript-formatter"))
  :hook
  ((typescript-mode . tide-setup)
   (typescript-mode . lsp-mode)
   (typescript-mode . tide-hl-identifier-mode)))

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
  (gopls . "go get golang.org/x/tools/gopls@latest")
  :hook
  ((before-save . gofmt-before-save)))

(use-package flycheck-swift
  :commands flycheck-swift-setup
  :hook (swift-mode . flycheck-swift-setup))


;; Mu4e
(defconst my-interesting-mail
  (concat " \(maildir:/fastmail/Inbox"
          " OR maildir:/gmail/Inbox"
          " OR maildir:/gmail/[Gmail]/.All\ Mail"
          " OR maildir:/gmail/[Gmail]/.Important"
          " OR maildir:/riseup/Inbox\)")
  "Interesting mail suffix.")

(defun no-auto-fill ()
  "Turn off 'auto-fill-mode'."
  (auto-fill-mode -1))

(require 'org-mu4e)
(require 'mu4e)

(setq-default mu4e-update-interval (* 60 5))
(setq-default mu4e-get-mail-command "mbsync -aq; true")
(setq-default mu4e-compose-dont-reply-to-self t)
(setq-default mu4e-user-mail-address-list '("thejefflarson@gmail.com"
                                            "thejefflarson@fastmail.com"
                                            "thejefflarson@riseup.net"))
(setq-default mu4e-context-policy 'pick-first)
(setq-default mu4e-maildir "~/.mail")
(setq-default mu4e-attachment-dir "~/Downloads")
(setq-default mu4e-headers-skip-duplicates t)
(setq-default mu4e-headers-visible-lines 20)
(setq-default mu4e-view-show-addresses 'long)
(setq-default mu4e-compose-in-new-frame t)
(setq-default mu4e-compose-complete-only-personal t)
(setq-default mu4e-change-filenames-when-moving t)
(setq-default message-kill-buffer-on-exit t)
(setq-default message-send-mail-function 'message-send-mail-with-sendmail)
(setq-default sendmail-program "/usr/local/bin/msmtp")
(setq-default message-sendmail-extra-arguments '("--read-envelope-from"))
(setq-default message-sendmail-f-is-evil t)
(setq-default mu4e-bookmarks
              `((,(concat "flag:unread date:today..now" my-interesting-mail)
                 "Today's unread messages" ?u)
                (,(concat "date:today..now" my-interesting-mail)
                 "Today's messages" ?t)
                (,(concat "date:7d..now" my-interesting-mail)
                 "This week's messages" ?w)
                (,my-interesting-mail
                 "All messages" ?a)))
(add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
(add-hook 'mu4e-compose-mode-hook 'visual-line-mode)
(add-hook 'mu4e-view-mode-hook 'epa-mail-mode)
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)
(add-hook 'mu4e-compose-mode-hook #'no-auto-fill)
(add-to-list 'mu4e-view-actions
             '("View In Browser" . mu4e-action-view-in-browser) t)

(require 'mu4e-contrib)
(setq-default mu4e-html2text-command 'mu4e-shr2text)

(use-package mu4e-maildirs-extension
  :custom
  (mu4e-maildirs-extension-fake-maildir-separator "\\.")
  :config
  (mu4e-maildirs-extension))

(use-package mu4e-alert
  :hook (after-init . mu4e-alert-enable-notifications)
  :config
  (setq mu4e-alert-set-default-style (if (eq system-type 'darwin)
                                         'notifier '(notifications)))

(require 'epg-config)

(setq-default mml2015-use 'epg
              mml2015-encrypt-to-self t
              mml2015-sign-with-sender t)

(require 'mu4e-context)

(setq-default mu4e-contexts
              `( ,(make-mu4e-context
                   :name "gmail"
                   :enter-func (lambda ()
                                 (mu4e-message "entering gmail"))
                   :match-func (lambda (msg)
                                 (when msg
                                   (string-match "gmail"
                                                 (mu4e-message-field msg :maildir))))
                   :vars '((mail-reply-to . "thejefflarson@gmail.com")
                           (user-mail-address . "thejefflarson@gmail.com")
                           (user-full-name . "Jeff Larson")
                           (mu4e-sent-messages-behavior . delete)
                           (mu4e-drafts-folder . "/gmail/[Gmail]/Drafts")
                           (mu4e-sent-folder . "/gmail/[Gmail]/Sent Mail")
                           (mu4e-trash-folder . "/gmail/[Gmail]/Trash")
                           (mu4e-refile-folder . "/gmail/[Gmail]/All Mail")))
                 ,(make-mu4e-context
                   :name "fastmail"
                   :enter-func (lambda ()
                                 (mu4e-message "entering fastmail"))
                   :match-func (lambda (msg)
                                 (when msg
                                   (string-match "fastmail"
                                                 (mu4e-message-field msg :maildir))))
                   :vars '((mail-reply-to . "thejefflarson@fastmail.com")
                           (user-mail-address . "thejefflarson@fastmail.com")
                           (user-full-name . "Jeff Larson")
                           (mu4e-sent-messages-behavior . sent)
                           (mu4e-drafts-folder . "/fastmail/Drafts")
                           (mu4e-sent-folder . "/fastmail/Sent")
                           (mu4e-trash-folder . "/fastmail/Trash")
                           (mu4e-refile-folder . "/fastmail/Archive")))
                 ,(make-mu4e-context
                   :name "riseup"
                   :enter-func (lambda ()
                                 (mu4e-message "entering riseup"))
                   :match-func (lambda (msg)
                                 (when msg
                                   (string-match "riseup"
                                                 (mu4e-message-field msg :maildir))))
                   :vars '((mail-reply-to . "thejefflarson@riseup.net")
                           (user-mail-address . "thejefflarson@riseup.net")
                           (user-full-name . "Jeff Larson")
                           (mu4e-sent-messages-behavior . sent)
                           (mu4e-drafts-folder . "/riseup/Drafts")
                           (mu4e-sent-folder . "/riseup/Sent")
                           (mu4e-trash-folder . "/riseup/Trash")
                           (mu4e-refile-folder . "/riseup/Archive")))
                 )
              )


(require 'mu4e-vars)

(add-to-list 'mu4e-header-info-custom
             '(:mail-directory .
                               (:name "Mail Directory"
                                :shortname "Dir"
                                :help "Mail Storage Directory"
                                :function
                                (lambda (msg)
                                  (or
                                   (mu4e-message-field msg :maildir) "")))))
(setq mu4e-headers-fields '((:mail-directory . 20)
                            (:human-date     . 12)
                            (:flags          .  6)
                            (:mailing-list   . 10)
                            (:from           . 22)
                            (:subject        . nil))))


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
