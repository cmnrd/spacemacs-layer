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
    ;; show new messages first
    (setq notmuch-search-oldest-first nil)

    :post-config

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

    (evilified-state-evilify-map notmuch-search-mode-map
      :mode notmuch-search-mode
      :bindings
      (kbd "a") 'notmuch-extra-search-archive-thread-down
      (kbd "A") 'notmuch-extra-search-archive-thread-up
      (kbd "d") 'spacemacs/notmuch-search-message-delete-down
      (kbd "D") 'spacemacs/notmuch-search-message-delete-up
      (kbd "J") 'notmuch-jump-search
      (kbd "L") 'notmuch-search-filter
      (kbd "g") 'notmuch-refresh-this-buffer
      (kbd "M") 'compose-mail-other-frame)))
