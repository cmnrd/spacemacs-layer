;; Copyright (c) 2021 Christian Menard
;;
;; Author: Christian Menard <christian.menard@gmx.de>
;;
;; This file is not part of GNU Emacs.

(defconst org-extra-packages
  '(vulpea org crux org-roam-bibtex org-fancy-priorities))

(defun org-extra/init-vulpea ()
  (use-package vulpea))

(defun org-extra/init-crux ()
  (use-package crux))

(defun org-extra/init-org-roam-bibtex ()
  (use-package org-roam-bibtex
    :after org-roam
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :config
    (require 'org-ref)))

(defun org-extra/init-org-fancy-priorities ()
  (use-package org-fancy-priorities
    :ensure t
    :hook
    (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '("A*" "A" "B" "C" "D")
          org-highest-priority 1
          org-default-priority 3
          org-fancy-priorities-list '((?1 . "A*") (?2 . "A") (?3 . "B") (?4 . "C") (?5 . "D")))))

(defun org-extra/post-init-org ()
  (add-to-list 'org-tags-exclude-from-inheritance "tags")
  (add-hook 'find-file-hook #'org-extra-update-tasks-tag)
  (add-hook 'before-save-hook #'org-extra-update-tasks-tag)
  ;; set the default archive command to org-archive-to-archive-sibling
  (setq org-archive-default-command 'org-archive-to-archive-sibling)
  (advice-add 'org-agenda :before #'org-extra-update-agenda-files)
  (setq org-agenda-prefix-format
        '((agenda . " %i %-32(org-extra-agenda-category 32)%?-22t% s")
          (todo . " %i %-32(org-extra-agenda-category 32) ")
          (tags . " %i %-32(org-extra-agenda-category 32) ")
          (search . " %i %-32(org-extra-agenda-category 32) ")))
  )
