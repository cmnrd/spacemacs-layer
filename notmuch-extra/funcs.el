;; Copyright (c) 2021 Christian Menard
;;
;; Author: Christian Menard <christian@gmx.de>
;;
;; This file is not part of GNU Emacs.


;; helper function to sort tags, this helps to put symbols before other tags
(defun notmuch-extra-sort-tags (tags)
  (setq res nil)
  (setq tail nil)
  (if (member "unread" tags)
      (setq tags (remove "unread" tags)
            res (append res '("unread"))))
  (if (member "inbox" tags)
      (setq tags (remove "inbox" tags)
            res (append res '("inbox"))))
  (if (member "work" tags)
      (setq tags (remove "work" tags)
            res (append res '("work"))))
  (if (member "signed" tags)
      (setq tags (remove "signed" tags)
            res (append res '("signed"))))
  (if (member "attachement" tags)
      (setq tags (remove "attachement" tags)
            res (append res '("attachement"))))
  (if (member "to-me" tags)
      (setq tags (remove "to-me" tags)
            res (append res '("to-me"))))
  (if (member "replied" tags)
      (setq tags (remove "replied" tags)
            tail (append tail '("replied"))))
  (if (member "sent" tags)
      (setq tags (remove "sent" tags)
            tail (append tail '("sent"))))
  (setq res (append res (sort tags #'string<)))
  (append res tail))
