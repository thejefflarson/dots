;;; jeffs-init --- various bits and boops to make emacs a nicer place

;;; Commentary:
;; Too many late nights copying and pasting from the internet,
;; but hey I'm proud of it.
;;; Code:

;; Configuration
(setq gc-cons-threshold 100000000)

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

(unless (and (file-exists-p user-cache-directory)
             (file-accessible-directory-p user-cache-directory))
  (mkdir user-cache-directory))

;; OS X specific configuration
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq mac-pass-command-to-system nil)
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

(when (eq system-type 'darwin)
  (progn
    (setq insert-directory-program "/usr/local/bin/gls")
    (setq dired-listing-switches "-aBhl --group-directories-first")))


;; Server code
(unless (string-equal "root" (getenv "USER"))
  (require 'server)
  (unless (server-running-p) (server-start)))

;; Packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'req-package)

(req-package diminish
  :config
  (diminish 'auto-revert-mode))

(req-package smooth-scrolling
  :config
  (smooth-scrolling t))

(req-package rainbow-delimiters
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package linum-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook 'linum-mode))

(req-package swiper
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (ivy-mode 1)
  (global-set-key (kbd "C-s") 'swiper))

(req-package counsel
  :require swiper
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
   ("C-x l" . counsel-locate)
   ("C-S-o" . counsel-rhythmbox)))

(req-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(req-package flycheck
  :commands (global-flycheck-mode)
  :init
  (add-hook 'after-init-hook 'global-flycheck-mode))

(req-package projectile
  :requires swiper
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

(req-package smartparens
  :diminish smartparens-mode
  :defer t)

(req-package smartparens-config
  :require smartparens
  :defer t
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode))

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
        eshell-directory-name (concat user-cache-directory "eshell/")))

(req-package shell-pop
  :bind (("C-t" . shell-pop))
  :init (setq shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell)))))

(req-package company
  :diminish company-mode
  :defer t
  :init
  (add-hook 'prog-mode-hook 'global-company-mode)
  :config
  (add-to-list 'company-backends 'company-c-headers))

(req-package company-c-headers
  :require company
  :defer t)

(req-package company-statistics
  :defer t
  :init
  (setq company-statistics-file (concat user-cache-directory
                                        "company-statistics-cache.el"))
  :config
  (add-hook 'after-init-hook 'company-statistics-mode))

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
  :init (add-hook 'text-mode-hook 'flyspell-mode))

(req-package neotree
  :bind ("M-t" . neotree)
  :config
  (setq neo-theme 'arrow)
  (setq neo-show-hidden-files t)
  (setq projectile-switch-project-action 'neotree-projectile-action))

(req-package twittering-mode
  :defer t)

(req-package vlf
  :defer t)

(req-package vlf-setup
  :require vlf)


;; Programming modes
(req-package ruby-mode
  :defer t
  :config
  ;; We never want to edit Rubinius bytecode
  (add-to-list 'completion-ignored-extensions ".rbc"))

(req-package projectile-rails
  :defer t
  :require projectile
  :commands (projectile-rails-on)
  :init
  (add-hook 'ruby-mode-hook 'projectile-rails-on))

(req-package cc-mode
  :defer t
  :config
  (setq-default c-basic-offset 2)
  (setq-default c-default-style "linux"))

(req-package gdb-mi
  :require cc-mode
  :defer t
  :config
  (setq gdb-many-windows t)
  (setq gdb-show-main t))

(req-package css-mode
  :require kurecolor rainbow-mode
  :defer t
  :config
  (rainbow-mode))

(req-package rust-mode
  :defer t)

(req-package fish-mode)


;; Mu4e
(defconst my-interesting-mail
  (concat " \(maildir:/work/INBOX"
          " OR maildir:/gmail/INBOX"
          " OR maildir:/gmail/[Gmail]/.All\ Mail"
          " OR maildir:/gmail/[Gmail]/.Important"
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
  (setq mu4e-headers-skip-duplicates t)
  (setq mu4e-headers-visible-lines 20)
  (setq mu4e-view-show-addresses 'long)
  (setq mu4e-compose-in-new-frame t)
  (setq mu4e-compose-complete-only-personal t)
  (setq message-kill-buffer-on-exit t)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/local/bin/msmtp")
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil 't)
  (setq mu4e-bookmarks
        `((,(concat "flag:unread date:today..now" my-interesting-mail) "Today's unread messages"  ?u)
          (,(concat "date:today..now" my-interesting-mail)             "Today's messages"         ?t)
          (,(concat "date:7d..now" my-interesting-mail)                "This week's messages"     ?w)
          (,my-interesting-mail                                         "All messages"            ?a)))
  :config
  (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
  (add-hook 'mu4e-compose-mode-hook 'visual-line-mode)
  (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode))

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
                             (string-match "gmail" (mu4e-message-field msg :maildir))))
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
                             (string-match "work" (mu4e-message-field msg :maildir))))
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
                             (string-match "riseup" (mu4e-message-field msg :maildir))))
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
    )

(req-package mu4e-vars
  :require mu4e
  :config
  (add-to-list 'mu4e-header-info-custom
               '(:mail-directory .
                                 (:name "Mail Directory"
                                        :shortname "Dir"
                                        :help "Mail Storage Directory"
                                        :function (lambda (msg)
                                                    (or (mu4e-message-field msg :maildir) "")))))
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
  (progn
    (when (eq system-type 'darwin)
      (set-face-attribute 'default nil :family "Monaco"))
    (when (eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :family "Source Code Pro"))
    (set-face-attribute 'default nil :height 120)))

(req-package-finish)
