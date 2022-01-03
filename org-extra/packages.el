;; Copyright (c) 2021 Christian Menard
;;
;; Author: Christian Menard <christian.menard@gmx.de>
;;
;; This file is not part of GNU Emacs.

(defconst org-extra-packages
  '(vulpea org))

(defun org-extra/init-vulpea ()
  (use-package vulpea))

(defun org-extra/post-init-org ()
  (add-to-list 'org-tags-exclude-from-inheritance "tags")
  (add-hook 'find-file-hook #'org-extra-update-tasks-tag)
  (add-hook 'before-save-hook #'org-extra-update-tasks-tag)
  )
