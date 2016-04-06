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
  '((mu4e :location built-in)
    (mu4e-context :location built-in)
    (mu4e-contrib :location built-in)))

(defun jeff-email/post-init-mu4e ()
  "Set up various email preferences."
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-update-interval (* 60 5))
  (setq mu4e-get-mail-command "offlineimap -u quiet; true")
  (setq mu4e-maildir "~/.mail/gmail")
  (setq mu4e-mu-home "~/.mail/mu-gmail")
  (add-hook 'mu4e-compose-mode-hook 'epa-mail-mode)
  (add-hook 'mu4e-view-mode-hook 'epa-mail-mode))

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
    ))

(defun jeff-email/init-mu4e-contrib ()
  (use-package mu4e-contrib
    :config
    (setq mu4e-html2text-command 'mu4e-shr2text)))
;;; packages.el ends here
