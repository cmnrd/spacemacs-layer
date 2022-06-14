(defconst kanban-packages '(org-kanban))

(defun kanban/init-org-kanban ()
  (use-package org-kanban))

(defun kanban/post-init-org-kanban ()
  (advice-add 'org-kanban//params-files :override #'kanban-get-roam-task-files))
