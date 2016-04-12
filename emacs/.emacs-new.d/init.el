
;; Configuration
(setq load-prefer-newer t)

;; Allow the -l flag to find the right emacs directory
(defconst user-emacs-directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Current emacs.d directory")

(defconst user-cache-directory
  (file-name-as-directory
   (concat user-emacs-directory ".cache"))
  "Directory for temporary files.")

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
(when (file-exists-p custom-file)
  (load-file custom-file))

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


;; Server code
(unless (string-equal "root" (getenv "USER"))
  (require 'server)
  (unless (server-running-p) (server-start)))


;; Builtin configuration
(eval-after-load 'srecode
  '(progn
     (setq srecode-map-save-file
           (concat user-cache-directory "srecode-map.el"))))


;; Packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(require 'req-package)
(req-package swiper)





;; Mu4e
(req-package mu4e
             :init
             (setq mu4e-update-interval (* 60 5))
             (setq mu4e-get-mail-command "offlineimap -u quiet; true")
             (setq mu4e-compose-dont-reply-to-self t)
             (setq mu4e-user-mail-address-list '("thejefflarson@gmail.com",
                                                 "jeff.larson@propublica.org",
                                                 "thejefflarson@riseup.net"))
             (setq mu4e-context-policy 'pick-first)
             (setq mu4e-maildir "~/.mail")
             (setq mu4e-headers-skip-duplicates t)
             (setq mu4e-headers-visible-lines 20)
             (setq mu4e-view-show-addresses 'long)
             (setq mu4e-compose-in-new-frame t)
             (setq message-send-mail-function 'message-send-mail-with-sendmail)
             (setq sendmail-program "/usr/local/bin/msmtp")
             (setq message-sendmail-extra-arguments '("--read-envelope-from"))
             (setq message-sendmail-f-is-evil 't)
             :config
             (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
             (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)
             (add-hook 'mu4e-view-mode-hook 'visual-line-mode))

(req-package mu4e-contrib
             :init (setq mu4e-html2text-command 'mu4e-shr2text))

(req-package mu4e-maildirs-extension
             :require mu4e
             :init (setq mu4e-maildirs-extension-fake-maildir-separator "\\.")
             :config (mu4e-maildirs-extension))

(req-package mu4e-alert
             :init
             (setq mu4e-alert-interesting-mail-query 
                   (concat 
                    "flag:unread date:today..now \("
                    "maildir:/work/INBOX"
                    " OR maildir:/gmail/INBOX"
                    " OR maildir:/gmail/[Gmail].All\ Mail"
                    " OR maildir:/gmail/[Gmail].Important"
                    " OR maildir:/riseup/INBOX\)"))
             :config
             (mu4e-alert-set-default-style (if (eq system-type 'darwin)
                                               'notifier 'notifications))
             (mu4e-alert-enable-notifications)
             (mu4e-alert-enable-mode-line-display))

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
                                (mu4e-drafts-folder . "/gmail/[Gmail].Drafts")
                                (mu4e-sent-folder . "/gmail/[Gmail].Sent Mail")
                                (mu4e-trash-folder . "/gmail/[Gmail].Trash")
                                (mu4e-refile-folder . "/gmail/[Gmail].All Mail")))
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
                                (mu4e-refile-folder . "/riseup/Archive"))))))

(req-package-finish)
