;; Copyright (c) 2021 Christian Menard
;;
;; Author: Christian Menard <christian@gmx.de>
;;
;; This file is not part of GNU Emacs.

(defconst notmuch-extra-packages
  '(notmuch
    all-the-icons))

(defun notmuch-extra/pre-init-all-the-icons ()
  ;; nothing to do
  )

(defun notmuch-extra/pre-init-notmuch ()
  (spacemacs|use-package-add-hook notmuch
    :post-init
    :post-config

    ;; show new messages first
    (setq notmuch-search-oldest-first nil)

    ;; configure sending via msmtp
    (setq message-send-mail-function 'message-send-mail-with-sendmail)
    (setq sendmail-program "/usr/local/bin/msmtp-enqueue.sh")
    (setq mail-specify-envelope-from t)
    (setq message-sendmail-envelope-from 'header)
    (setq mail-envelope-from 'header)

    ;; use icons to visualize often used tags
    (setq notmuch-tag-formats `(("unread" (all-the-icons-octicon "mail"))
                                ("inbox" (all-the-icons-octicon "inbox"))
                                ("replied" (all-the-icons-octicon "mail-reply"))
                                ("sent" (all-the-icons-octicon "arrow-up"))
                                ("to-me" (all-the-icons-octicon "person"))
                                ("signed" (all-the-icons-octicon "pencil"))
                                ("work" (all-the-icons-octicon "briefcase"))
                                ("attachment" (all-the-icons-octicon "package"))))

    ;; replace notmuch's tag format function to display icons before text tags
    (defun notmuch-tag-format-tags (tags orig-tags &optional face)
      "Return a string representing formatted TAGS."
      (let ((face (or face 'notmuch-tag-face))
            (all-tags (notmuch-extra-sort-tags (delete-dups (append tags orig-tags nil)))))
        (notmuch-apply-face
         (mapconcat #'identity
                    ;; nil indicated that the tag was deliberately hidden
                    (delq nil (mapcar
                               (apply-partially #'notmuch-tag-format-tag tags orig-tags)
                               all-tags))
                    " ")
         face
         t)))

    ;; define additional key bindings
    (define-key notmuch-show-mode-map "d"
      (lambda ()
        "toggle deleted tag for thread"
        (interactive)
        (if (member "deleted" (notmuch-show-get-tags))
            (notmuch-show-tag (list "-deleted"))
          (notmuch-show-tag (list "+deleted")))))
    (define-key notmuch-search-mode-map "d"
      (lambda ()
        "toggle deleted tag for thread"
        (interactive)
        (if (member "deleted" (notmuch-search-get-tags))
            (notmuch-search-tag (list "-deleted"))
          (notmuch-search-tag (list "+deleted")))))

    (define-key notmuch-show-mode-map "a"
      (lambda ()
        "archive message"
        (interactive)
        (notmuch-show-tag (list "-unread" "-inbox"))))
    (define-key notmuch-search-mode-map "a"
      (lambda ()
        "archive thread"
        (interactive)
        (notmuch-search-tag (list "-unread" "-inbox"))))

    (define-key notmuch-show-mode-map "S"
      (lambda ()
        "mark message as spam"
        (interactive)
        (notmuch-show-tag (list "+spam"))))
    (define-key notmuch-search-mode-map "S"
      (lambda ()
        "mark thread as spam"
        (interactive)
        (notmuch-search-tag (list "+spam"))))

    (define-key notmuch-show-mode-map "u"
      (lambda ()
        "toggle unread tag for message"
        (interactive)
        (if (member "unread" (notmuch-show-get-tags))
            (notmuch-show-tag (list "-unread"))
          (notmuch-show-tag (list "+unread")))))
    (define-key notmuch-search-mode-map "u"
      (lambda ()
        "toggle unread tag for message"
        (interactive)
        (if (member "unread" (notmuch-search-get-tags))
            (notmuch-search-tag (list "-unread"))
          (notmuch-search-tag (list "+unread")))))

    (define-key notmuch-search-mode-map "g"
      'notmuch-poll-and-refresh-this-buffer)
    (define-key notmuch-hello-mode-map "g"
      'notmuch-poll-and-refresh-this-buffer)))
