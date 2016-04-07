;;; packages.el --- jeff-email layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Jeff Larson <jeff@quiet-car.jeffl.es>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; General email tomfoolery.
;;
;;; Code:

(defconst jeff-email-packages
  '((mu4e :location built-in)
    (mu4e-context :location built-in)
    (mu4e-contrib :location built-in)
    (mu4e-maildirs-extension :location elpa)))

(defun jeff-email/post-init-mu4e ()
  "Set up various email preferences."
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-update-interval (* 60 10))
  (setq mu4e-get-mail-command "offlineimap -u quiet; true")
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-maildir "~/.mail")
  (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
  (add-hook 'mu4e-view-mode-hook 'epa-mail-mode)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/local/bin/msmtp")
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil 't))

(defun jeff-email/init-mu4e-context ()
  "Define different email contexts"
  (use-package mu4e-context
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
                       (mu4e-drafts-folder . "/riseup/Drafts")
                       (mu4e-sent-folder . "/riseup/Sent")
                       (mu4e-trash-folder . "/riseup/Trash")
                       (mu4e-refile-folder . "/riseup/Archive")))
             )
          )
    )
  )

(defun jeff-email/init-mu4e-contrib ()
  "Use shr2text for HTML emails"
  (use-package mu4e-contrib
    :config
    (setq mu4e-html2text-command 'mu4e-shr2text)))

(defun jeff-email/init-mu4e-maildirs-extension ()
  (use-package mu4e-maildirs-extension
    "Grab unread / total counts by email box"
    :init
    (mu4e-maildirs-extension)
    :config
    (setq mu4e-maildirs-extension-fake-maildir-separator "\\.")))
;;; packages.el ends here
