;;; jeffs-init --- various bits and boops to make emacs a nicer place

;;; Commentary:
;; Too many late nights copying and pasting from the internet,
;; but hey I'm proud of it.
;;; Code:

;; Configuration

(setq gc-cons-threshold 100000000)

(setq-default indent-tabs-mode nil)

(setq-default fill-column 80)

(setq user-full-name "Jeff Larson"
      user-mail-address "thejefflarson@gmail.com")

(setq load-prefer-newer t)

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
  (define-key global-map [home] 'beginning-of-line)
  (define-key global-map [end] 'end-of-line)
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

(global-auto-revert-mode t)

(when (eq system-type 'darwin)
  (progn
    (setq insert-directory-program "/usr/local/bin/gls")
    (setq dired-listing-switches "-aBhl --group-directories-first")))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(show-paren-mode 1)
(electric-pair-mode 1)
(global-set-key (kbd "C-\\") 'comment-or-uncomment-region)
(setq-default abbrev-mode -1)


;; Server code
(unless (string-equal "root" (getenv "USER"))
  (require 'server)
  (unless (server-running-p) (server-start)))

;; Packages
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'req-package)

(req-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(req-package whitespace
  :defer t
  :diminish t
  :commands (whitepace-mode)
  :init
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (setq whitespace-style '(face lines-tail)))

(req-package diminish
  :config
  (diminish 'auto-revert-mode))

(req-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package linum-mode
  :defer t)

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
   ("C-x C-f" .  counsel-find-file)
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
  :require counsel
  :config
  (global-set-key (kbd "C-s") 'swiper))

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
                        '(javascript-jshint))))

(req-package ag
  :defer t)

(req-package projectile
  :requires swiper ag
  :defer t
  :commands projectile-mode projectile-global-mode
  :init
  (add-hook 'prog-mode-hook 'projectile-mode)
  (add-hook 'after-init-hook 'projectile-global-mode)
  :config
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t))

(req-package magit
  :requires swiper
  :bind (("M-m s" . magit-status))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

(req-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(req-package kurecolor
  :defer t)

(req-package rainbow-mode
  :defer t
  :requires kurecolor)

(req-package diff-hl
  :require magit
  :commands (diff-hl-magit-post-refresh)
  :init
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode 1))

(req-package page-break-lines
  :diminish page-break-lines-mode
  :commands (global-page-break-lines-mode page-break-lines-mode)
  :init
  (add-hook 'after-init-hook 'global-page-break-lines-mode)
  (add-hook 'prog-mode-hook 'page-break-lines-mode))

(req-package winner
  :config
  (winner-mode t))

(req-package eshell
  :defer t
  :config
  (setq eshell-buffer-maximum-lines 20000
        eshell-history-size 350
        eshell-hist-ignoredups t
        eshell-plain-echo-behavior t
        eshell-directory-name
        (concat user-cache-directory "eshell/")))

(req-package shell-pop
  :bind (("C-t" . shell-pop))
  :init (setq shell-pop-shell-type
              '("eshell" "*eshell*" (lambda () (eshell)))))

(req-package company
  :diminish company-mode
  :defer t
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
  :config
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
  :config
  (company-statistics-mode))

(defun enable-semantic ()
  "Set up semantic in programming modes."
  (require 'semantic)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-mode 1))

(req-package semantic
  :defer t
  :init
  (setq srecode-map-save-file
        (concat user-cache-directory "srecode-map.el"))
  (add-hook 'prog-mode-hook 'enable-semantic))

(defun enable-srefactor ()
  "Set up srefactor."
  (require 'srefactor))

(req-package srefactor
  :require semantic cc-mode
  :defer t
  :init
  (add-hook 'c-mode-hook 'enable-srefactor)
  (add-hook 'c++-mode-hook 'enable-srefactor)
  (add-hook 'elisp-mode-hook 'enable-srefactor))

(req-package flyspell
  :commands (flyspell-buffer flyspell-mode)
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'flyspell-mode-hook #'flyspell-popup-auto-correct-mode))

(req-package flyspell-popup
  :commands (flyspell-popup-auto-correct-mode)
  :require flyspell)

(req-package neotree
  :bind ("M-t" . neotree)
  :commands (neotree-find)
  :require projectile
  :init
  (setq neo-theme 'arrow)
  (setq neo-show-hidden-files t)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (custom-set-faces
   '(neo-banner-face ((t . (:inherit shadow))) t)
   '(neo-header-face ((t . (:inherit shadow))) t)
   '(neo-root-dir-face ((t . (:inherit link-visited :underline nil))) t)
   '(neo-dir-link-face ((t . (:inherit dired-directory))) t)
   '(neo-file-link-face ((t . (:inherit default))) t)
   '(neo-button-face ((t . (:inherit dired-directory))) t)
   '(neo-expand-btn-face ((t . (:inherit button))) t)))

(defun neotree-find-project-root ()
  "Find the root of a project for NeoTree using projectile."
  (interactive)
  (let ((origin-buffer-file-name (buffer-file-name)))
    (neotree-find (projectile-project-root))
    (neotree-find origin-buffer-file-name)))

(req-package twittering-mode
  :defer t)

(req-package vlf
  :defer t)

(req-package vlf-setup
  :require vlf)

(setq org-support-shift-select t)
(ensure-directory "~/SpiderOak Hive/journal/")
(req-package org-journal
  :bind (("C-c C-j" . org-journal-new-entry))
  :config
  (setq org-journal-dir "~/SpiderOak Hive/journal/"))

(req-package ecb
  :commands ecb-activate)

(req-package cider
  :defer t
  :bind (("C-u M-x" . cider-jack-in))
  :require clojure-mode)

(req-package writeroom-mode
  :defer t)


;; Programming modes
(req-package ruby-mode
  :defer t
  :config
  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc"))

(req-package projectile-rails
  :defer t
  :require projectile
  :commands projectile-rails-on
  :init
  (add-hook 'ruby-mode-hook 'projectile-rails-on))

(req-package cc-mode
  :defer t
  :config
  (setq-default c-basic-offset 2)
  (setq-default c-default-style "linux"))

(req-package platformio-mode
  :defer t
  :require irony company
  :commands platformio-conditionally-enable
  :init
  (add-hook 'c++-mode-hook 'platformio-conditionally-enable))

(req-package gdb-mi
  :require cc-mode
  :commands gdb
  :defer t
  :config
  (setq gdb-many-windows t)
  (setq gdb-show-main t))

(req-package css-mode
  :require kurecolor rainbow-mode
  :defer t
  :init
  (add-hook 'css-mode-hook 'rainbow-mode))

(req-package clojure-mode
  :defer t)

(req-package racer
  :defer t
  :commands racer-mode
  :init
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  (add-hook 'racer-mode-hook 'company-mode)
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t)
  (setq racer-cmd "~/.cargo/bin/racer"))

(req-package flycheck-rust
  :defer t
  :commands flycheck-rust-setup)

(req-package rust-mode
  :defer t
  :require racer flycheck-rust
  :config
  (progn
    (racer-activate)
    (racer-turn-on-eldoc)
    (flycheck-rust-setup)))

(req-package fish-mode)
(req-package web-mode
  :defer t
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.jsx\\'" . web-mode))
  :config
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-part-face t)
  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(req-package js2-mode
  :defer t
  :mode
  (("\\.js\\'" . js2-mode))
  :config
  (setq js2-basic-offset 2))

(req-package tern
  :defer t
  :init
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(req-package sql-indent
  :defer t)

(req-package sql
  :mode
  (("\\.sql\\'" . sql-mode))
  :config
  (load-library "sql-indent"))

(req-package json-mode
  :defer t)

(req-package scss-mode
  :defer t
  :mode
  (("\\.scss\\'" . scss-mode))
  :config
  (setq scss-compile-at-save nil))

(req-package markdown-mode
  :commands
  (markdown-mode gfm-mode)
  :mode
  (("README\\.md\\'" . gfm-mode)
   ("\\.md\\'" . markdown-mode)
   ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(req-package bison-mode
  :defer t
  :mode
  (("\\.l\\'" . bison-mode))
  (("\\.y\\'" . bison-mode)))


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
  (setq message-sendmail-f-is-evil 't)
  (setq mu4e-bookmarks
        `((,(concat "flag:unread date:today..now" my-interesting-mail)
           "Today's unread messages" ?u)
          (,(concat "date:today..now" my-interesting-mail)
           "Today's messages" ?t)
          (,(concat "date:7d..now" my-interesting-mail)
           "This week's messages" ?w)
          (,my-interesting-mail
           "All messages" ?a)))
  :config
  (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
  (add-hook 'mu4e-compose-mode-hook 'visual-line-mode)
  (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t))

(req-package mu4e-contrib
  :require mu4e
  :init
  (setq mu4e-html2text-command 'mu4e-shr2text))

(req-package mu4e-maildirs-extension
  :require mu4e
  :init (setq mu4e-maildirs-extension-fake-maildir-separator "\\.")
  :config (mu4e-maildirs-extension))

(req-package mu4e-alert
  :require mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread date:today..now"
         my-interesting-mail))
  (add-hook 'after-init-hook 'mu4e-alert-enable-notifications)
  (add-hook 'after-init-hook 'mu4e-alert-enable-mode-line-display)
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
                     (mu4e-refile-folder . "/work/archive")))
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

(req-package color-theme-sanityinc-tomorrow
  :config
  (color-theme-sanityinc-tomorrow-eighties))

(req-package faces
  :config
    (when (eq system-type 'darwin)
      (set-face-attribute 'default nil :family "Monaco")
      (set-face-attribute 'default nil :height 120))
    (when (eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :family "Source Code Pro")
      (set-face-attribute 'default nil :height 100)))

(req-package-finish)
