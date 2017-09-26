;;; jeffs-init --- various bits and boops to make emacs a nicer place

;;; Commentary:
;; Too many late nights copying and pasting from the internet,
;; but hey I'm proud of it.
;;; Code:

;; Configuration
(setq gc-cons-threshold (eval-when-compile (* 100 1024 1024)))

(setq-default indent-tabs-mode nil
              fill-column 100)

(setq user-full-name "Jeff Larson"
      user-mail-address "thejefflarson@gmail.com"
      load-prefer-newer t
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil)

(setq window-divider-default-places t
      window-divider-default-bottom-width 1
      window-divider-default-right-width 1)
(window-divider-mode)
(tooltip-mode -1)
;; Allow the -l flag to find the right emacs directory
(defconst user-emacs-directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Current emacs.d directory.")

(defconst user-cache-directory
  (file-name-as-directory
   (concat user-emacs-directory ".cache"))
  "Directory for temporary files.")

(defun ensure-directory (dir)
  "Make directory DIR if it doesn't exist."
  (unless (and (file-exists-p dir)
               (file-accessible-directory-p dir))
    (mkdir dir)))

(ensure-directory user-cache-directory)

;; OS X specific configuration
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq mac-pass-command-to-system nil)
  (setq mac-emulate-three-button-mouse t)
  (define-key key-translation-map (kbd "<s-mouse-1>") (kbd "<mouse-2>"))
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)
  (setq-default epg-gpg-program  "/usr/local/bin/gpg")
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
(blink-cursor-mode -1)
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

;; Use gnu ls on darwin
(when (eq system-type 'darwin)
  (setq insert-directory-program "/usr/local/bin/gls")
  (setq dired-listing-switches "-aBhl --group-directories-first"))

;; other niceties
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook 'whitespace-mode)
(setq-default whitespace-style '(face trailing lines-tail))
(setq-default whitespace-line-column 100)
(show-paren-mode 1)
(electric-pair-mode 1)
(recentf-mode)
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region)
(delete-selection-mode 1)
(windmove-default-keybindings 'super)

(setq-default abbrev-mode -1)
(setq-default doc-view-resolution 300)

;; Blinky hairline cursor
(setq-default cursor-type '(bar . 1))
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
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(setq-default use-package-verbose t)

(require 'req-package)

(req-package 'epa-file
  :config
  (epa-file-enable))

(req-package 'buffer-move)

(req-package 'session
  :init
  (setq session-save-file (expand-file-name ".session" user-emacs-directory))
  (add-hook 'after-init-hook 'session-initialize))

(req-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(req-package crux
  :bind
  (("C-c o" . crux-open-with)
   ("C-k" . crux-smart-kill-line)
   ("C-c n" . crux-cleanup-buffer-or-region)
   ("C-c f" . crux-recentf-find-file)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c r" . crux-rename-file-and-buffer)
   ("C-c I" . crux-find-user-init-file)
   ("C-c i" . crux-ispell-word-then-abbrev))
  :config
  (crux-reopen-as-root-mode))

(req-package diminish
  :config
  (diminish 'auto-revert-mode))

(req-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package ivy
  :commands ivy-mode
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1))

(req-package counsel
  :require ivy
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-load-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("C-x l" . counsel-locate)))

(req-package swiper
  :require counsel ivy
  :bind
  (("C-s" . swiper)))

(req-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(req-package flycheck
  :commands (flycheck-mode)
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)
                        '(python-flake8))))

(req-package ag
  :defer t)

(req-package ripgrep
  :defer t)

(req-package projectile-ripgrep
  :defer t)

(req-package counsel-projectile
  :config
  (counsel-projectile-on))

(req-package neotree
  :commands neotree-projectile-action
  :init
  (setq neo-show-hidden-files t)
  (setq neo-window-fixed-size nil))

(setq-default projectile-completion-system 'ivy)
(setq-default projectile-enable-caching t)
(setq-default projectile-switch-project-action 'neotree-projectile-action)
(req-package projectile
  :requires swiper ag counsel-projectile neotree projectile-ripgrep)
(projectile-mode)

(req-package magit
  :require swiper
  :bind (("C-c s" . magit-status))
  :init
  (setq magit-completing-read-function 'ivy-completing-read))

(req-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(req-package kurecolor
  :defer t)

(req-package rainbow-mode
  :defer t
  :require kurecolor)

(req-package diff-hl
  :require magit
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode 1))

(req-package page-break-lines
  :diminish page-break-lines-mode
  :commands global-page-break-lines-mode page-break-lines-mode
  :init
  (add-hook 'after-init-hook 'global-page-break-lines-mode)
  (add-hook 'prog-mode-hook 'page-break-lines-mode))

(req-package winner
  :config
  (winner-mode t))

(req-package eshell
  :defer t
  :init
  (setq eshell-buffer-maximum-lines 20000
        eshell-history-size 350
        eshell-hist-ignoredups t
        eshell-plain-echo-behavior t
        eshell-directory-name
        (concat user-cache-directory "eshell/")))

(req-package shell-pop
  :bind (("C-t" . shell-pop))
  :init
  (setq shell-pop-shell-type
        '("eshell" "*eshell*" (lambda () (eshell)))))

(req-package company
  :diminish company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  :config
  (add-to-list 'company-backends 'company-irony))

(req-package irony
  :diminish irony-mode
  :defer t
  :commands flycheck-irony-setup
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(req-package flycheck-irony
  :require flycheck irony
  :defer t
  :commands flycheck-irony-mode
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(req-package company-irony
  :require company irony
  :defer t)

(req-package company-statistics
  :require company
  :diminish t
  :init
  (setq company-statistics-file
        (concat user-cache-directory
                "company-statistics-cache.el"))
  (add-hook 'after-init-hook 'company-statistics-mode))

(req-package nlinum
  :commands (nlinum-mode)
  :init
  (add-hook 'prog-mode-hook 'nlinum-mode))

(req-package hl-line-mode
  :defer t)

(req-package flyspell
  :commands (flyspell-buffer flyspell-mode)
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode))

(req-package flyspell-popup
  :commands (flyspell-popup-auto-correct-mode)
  :require flyspell)

(req-package twittering-mode
  :defer t)

(req-package vlf-setup
  :init
  (setq vlf-application 'dont-ask))

(req-package artbollocks-mode
  :config
  (add-hook 'text-mode-hook 'artbollocks-mode)
  (add-hook 'org-mode-hook 'artbollocks-mode))

(req-package org
  :require epa-file
  :commands org-agenda-list
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :init
  (load-library "~/.emacs.d/secrets.el.gpg")
  (ensure-directory "~/SpiderOak Hive/org/")
  (ensure-directory "~/SpiderOak Hive/journal/")
  (setq org-log-redeadline 'note)
  (setq org-log-reschedule 'note)
  (setq org-log-refile 'time)
  (setq org-use-tag-inheritance t)
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-mobile-inbox-for-pull "~/SpiderOak Hive/org/notes.org")
  (setq org-directory "~/SpiderOak Hive/org")
  (setq org-agenda-files (list "~/SpiderOak Hive/org/work.org"
                               "~/SpiderOak Hive/org/family.org"))
  (setq org-default-notes-file "~/SpiderOak Hive/org/notes.org")
  (setq org-agenda-window-setup 'only-window)
  (setq org-hierarchical-todo-statistics nil)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/SpiderOak Hive/org/work.org" "Tasks")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
          ("n" "Note" entry (file "~/SpiderOak Hive/org/notes.org")
           "* %?\nCaptured %<%Y-%m-%d %H:%M>")))
  (setq org-tag-alist '((:startgroup . nil)
                        ("@work" . ?w) ("@home" . ?h)
                        (:endgroup . nil)
                        ("phone" . ?p) ("meeting" . ?m)
                        ("code" . ?c) ("writing" . ?r)))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i!)" "WAITING(w@/!)" "|" "DONE(d@)" "CANCELED(c@)")))
  (setq org-enforce-todo-dependencies t)
  (setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  :config
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-mobile))

(req-package org-alert
  :require org
  :config
  (org-alert-enable))

(req-package org-projectile
  :require org
  :bind
  (("C-c n p" . org-projectile:project-todo-completing-read))
  :config
  (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
  (org-projectile:prompt))

(req-package org-journal
  :bind
  ("C-c C-j" . org-journal-new-entry)
  :init
  (setq org-journal-dir "~/SpiderOak Hive/journal/")
  (setq org-support-shift-select t))

(req-package ecb
  :commands ecb-activate)

(req-package cider
  :require clojure-mode
  :defer t
  :bind
  ("C-u M-x" . cider-jack-in))

(req-package writeroom-mode
  :defer t)

(req-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))


;; Programming modes
(req-package ruby-mode
  :require flycheck
  :mode "\\.rb\\'"
  :init
  (add-to-list 'completion-ignored-extensions ".rbc")
  :config
  (setq flycheck-rubocoprc nil))

(req-package projectile-rails
  :defer t
  :require projectile
  :commands projectile-rails-on
  :init
  (add-hook 'ruby-mode-hook 'projectile-rails-on))

(req-package company-jedi
  :defer t)

(req-package elpy
  :require company-jedi
  :init
  (setq yas-snippet-dirs nil)
  :config
  (elpy-enable))

(req-package pyvenv
  :commands pyvenv-activate)

(req-package cc-mode
  :defer t
  :init
  (setq-default c-basic-offset 2)
  (setq-default c-default-style "linux"))

(req-package platformio-mode
  :defer t
  :require irony company
  :commands platformio-conditionally-enable
  :init
  (add-hook 'c++-mode-hook 'platformio-conditionally-enable))

;; dunno why this needs to be non async but ok
(defun colorize-compilation-buffer()
  "Color compilation."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(req-package ansi-color
  :init
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(req-package gdb-mi
  :require cc-mode
  :commands gdb
  :defer t
  :init
  (setq gdb-many-windows t)
  (setq gdb-show-main t))

(req-package css-mode
  :mode "\\.css\\'"
  :require kurecolor rainbow-mode
  :init
  (add-hook 'css-mode-hook 'rainbow-mode))

(req-package csv-mode
  :mode "\\.csv\\'")

(req-package clojure-mode
  :mode "\\.clj\\'")

(req-package racer
  :commands racer-mode
  :defer t
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  (setq company-tooltip-align-annotations t)
  (setq racer-cmd "~/.cargo/bin/racer")
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common))

(req-package flycheck-rust
  :commands flycheck-rust-setup
  :init
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup))

(req-package rust-mode
  :require racer flycheck-rust
  :mode "\\.rs\\'"
  :init
  (setq rust-format-on-save t)
  (setq-local eldoc-documentation-function #'ignore))

(req-package toml-mode
  :mode "\\.toml\\'")

(req-package cargo
  :require rust-mode
  :commands cargo-minor-mode
  :config
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(req-package fish-mode
  :mode "\\.fish\\'")

(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)

(req-package web-mode
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.erb\\'" . web-mode))
  :init
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(req-package js2-mode
  :mode
  (("\\.js\\'" . js2-mode))
  (("\\.jsx\\'" . js2-jsx-mode))
  :init
  (setq js2-basic-offset 2)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(req-package json-mode
  :mode "\\.json\\'")

(req-package tern
  :defer t
  :init
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  :config
  (setq tern-command (append tern-command '("--no-port-file"))))

(req-package company-tern
  :require tern
  :defer t
  (add-to-list 'company-backend 'company-tern))

(req-package sql-indent
  :defer t)

(req-package sql
  :require sql-indent
  :mode "\\.sql\\'"
  :init
  (setq sql-indent-offset 2))

(req-package lua-mode
  :mode "\\.lua$")

(req-package scss-mode
  :mode "\\.scss\\'"
  :init
  (setq scss-compile-at-save nil))

(req-package markdown-mode
  :commands markdown-mode gfm-mode
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(req-package bison-mode
  :mode
  (("\\.l\\'" . bison-mode))
  (("\\.y\\'" . bison-mode)))

(req-package swift-mode
  :mode "\\.swift\\'")

(req-package go-mode
  :mode "\\.go\\'")

(req-package flycheck-swift
  :commands flycheck-swift-setup
  :init
  (add-hook 'swift-mode-hook 'flycheck-swift-setup))

(when (eq system-type 'darwin)
  (req-package company-sourcekit
    :require swift-mode
    :init
    (add-to-list 'company-backends 'company-sourcekit)))


;; Mu4e
(defconst my-interesting-mail
  (concat " \(maildir:/work/INBOX"
          " OR maildir:/gmail/INBOX"
          " OR maildir:/gmail/[Gmail]/.All\ Mail"
          " OR maildir:/gmail/[Gmail]/.Important"
          " OR maildir:/columbia/INBOX"
          " OR maildir:/columbia/[Gmail]/.All\ Mail"
          " OR maildir:/columbia/[Gmail]/.Important"
          " OR maildir:/riseup/INBOX\)")
  "Interesting mail suffix.")

(defun no-auto-fill ()
  "Turn off 'auto-fill-mode'."
  (auto-fill-mode -1))

(req-package org-mu4e
  :require org)

(req-package mu4e
  :init
  (setq mu4e-update-interval (* 60 5))
  (setq mu4e-get-mail-command "mbsync -aq; true")
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-user-mail-address-list '("thejefflarson@gmail.com"
                                      "jeff.larson@propublica.org"
                                      "thejefflarson@riseup.net"))
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-maildir "~/.mail")
  (setq mu4e-attachment-dir "~/Downloads")
  (setq mu4e-headers-skip-duplicates t)
  (setq mu4e-headers-visible-lines 20)
  (setq mu4e-view-show-addresses 'long)
  (setq mu4e-compose-in-new-frame t)
  (setq mu4e-compose-complete-only-personal t)
  (setq mu4e-change-filenames-when-moving t)
  (setq message-kill-buffer-on-exit t)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/local/bin/msmtp")
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil t)
  (setq mu4e-bookmarks
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
  :config
  (add-to-list 'mu4e-view-actions
               '("View In Browser" . mu4e-action-view-in-browser) t))

(req-package mu4e-contrib
  :require mu4e
  :init
  (setq mu4e-html2text-command 'mu4e-shr2text))

(req-package mu4e-maildirs-extension
  :require mu4e
  :init
  (setq mu4e-maildirs-extension-fake-maildir-separator "\\.")
  :config
  (mu4e-maildirs-extension))

(req-package mu4e-alert
  :require mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread date:today..now"
         my-interesting-mail))
  (add-hook 'after-init-hook 'mu4e-alert-enable-notifications)
  :config
  (mu4e-alert-set-default-style (if (eq system-type 'darwin)
                                    'notifier 'notifications)))

(req-package epg-config
  :init
  (setq mml2015-use 'epg
        mml2015-encrypt-to-self t
        mml2015-sign-with-sender t))

(req-package mu4e-context
  :config
  (setq mu4e-contexts
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
                     (mu4e-drafts-folder . "/gmail/[Gmail]/.Drafts")
                     (mu4e-sent-folder . "/gmail/[Gmail]/.Sent Mail")
                     (mu4e-trash-folder . "/gmail/[Gmail]/.Trash")
                     (mu4e-refile-folder . "/gmail/[Gmail]/.All Mail")))
           ,(make-mu4e-context
             :name "work"
             :enter-func (lambda ()
                           (mu4e-message "entering work"))
             :match-func (lambda (msg)
                           (when msg
                             (string-match "work"
                                           (mu4e-message-field msg :maildir))))
             :vars '((mail-reply-to . "jeff.larson@propublica.org")
                     (user-mail-address . "jeff.larson@propublica.org")
                     (user-full-name . "Jeff Larson")
                     (mu4e-sent-messages-behavior . sent)
                     (mu4e-drafts-folder . "/work/Drafts")
                     (mu4e-sent-folder . "/work/Sent")
                     (mu4e-trash-folder . "/work/Trash")
                     (mu4e-refile-folder . "/work/archive/.2017")))
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
           ,(make-mu4e-context
             :name "columbia"
             :enter-func (lambda ()
                           (mu4e-message "entering columbia"))
             :match-func (lambda (msg)
                           (when msg
                             (string-match "columbia"
                                           (mu4e-message-field msg :maildir))))
             :vars '((mail-reply-to . "jal2301@columbia.edu")
                     (user-mail-address . "jal2301@columbia.edu")
                     (user-full-name . "Jeff Larson")
                     (mu4e-sent-messages-behavior . delete)
                     (mu4e-drafts-folder . "/columbia/[Gmail]/.Drafts")
                     (mu4e-sent-folder . "/columbia/[Gmail]/.Sent Mail")
                     (mu4e-trash-folder . "/columbia/[Gmail]/.Trash")
                     (mu4e-refile-folder . "/columbia/[Gmail]/.All Mail")))
           )
        )
    )

(req-package mu4e-vars
  :require mu4e
  :config
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


;; Theme
(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(req-package all-the-icons)

(req-package doom-themes
  :require all-the-icons
  :config
  (doom-themes-neotree-config)
  (load-theme 'doom-vibrant t)
  (doom-themes-org-config))

(req-package solaire-mode
  :init
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'after-revert-hook #'turn-on-solaire-mode))

(setq-default org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(req-package faces
  :config
    (when (eq system-type 'darwin)
      (set-face-attribute 'default nil :family "Monaco")
      (set-face-attribute 'default nil :height 120))
    (when (eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :family "Source Code Pro")
      (set-face-attribute 'default nil :height 100)))

(req-package-finish)
(message "Loaded in `%s'" (emacs-init-time))

(defun layout()
  (interactive)
  (select-frame (make-frame '((user-position . t)
                              (width . 120)
                              (height . 40)
                              (top . -1)
                              (left . -1))))
  (org-agenda-list)
  (select-frame (make-frame '((user-position . t)
                              (width . 120)
                              (height . 40)
                              (top . 0)
                              (left . -1))))
  (mu4e))

(provide 'init)
;;; init.el ends here
