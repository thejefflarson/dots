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

;;; Commentary:

;; This is me trying to create a custom Spacemacs layer.

;;; Code:

(defconst jeff-email-packages
  '((mu4e :location built-in)))

(defun jeff-email/post-init-mu4e ()
  "Set up various email accounts."
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-maildir "~/.mail/gmail")
  (setq mu4e-mu-home "~/.mail/mu-gmail")
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "gmail"
             :enter-func (lambda ()
                           (mu4e-message "entering gmail"))
             :match-func (lambda (msg)
                           (if msg (string-equal "home" (mu4e-context-name mu4e~context-current)) t))
             :vars '((mail-reply-to . "thejefflarson@gmail.com")
                     (user-mail-address . "thejefflarson@gmail.com")
                     (user-full-name . "Jeff Larson")
                     (mu4e-drafts-folder . "/[Gmail].Drafts")
                     (mu4e-sent-folder . "/[Gmail].Sent Mail")
                     (mu4e-trash-folder . "/[Gmail].Trash")
                     (mu4e-maildir ."~/.mail/gmail")
                     (mu4e-mu-home . "~/.mail/mu-gmail")
                     (mu4e-refile-folder . "/[Gmail].All Mail"))))
        )
  (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
  (add-hook 'mu4e-view-mode-hook 'epa-mail-mode))

;;; packages.el ends here
