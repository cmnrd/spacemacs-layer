;; Copyright (c) 2021 Christian Menard
;;
;; Author: Christian Menard <christian.menard@gmx.de>
;;
;; This file is not part of GNU Emacs.


(defun org-extra-is-archivedp (h)
  "Recursively check if the heading has the ARCHIVE tag"
  (when h
    (or (org-element-property :archivedp h)
        (org-extra-is-archivedp (org-element-property :parent h)))))

(defun org-extra-contains-tasks-p ()
  "Return non-nil if current buffer has any task entries.

  This function ignores any archived tasks."
  (interactive)
  (org-element-map                          ; (2)
      (org-element-parse-buffer 'headline)  ; (1)
      'headline
    (lambda (h)
      (and (not (org-extra-is-archivedp h))
           (or (eq (org-element-property :todo-type h) 'todo)
               (eq (org-element-property :todo-type h) 'done)
               )))
      nil 'first-match))                      ; (3)

(defun org-extra-update-tasks-tag ()
  "Update tasks tag in the current buffer."
  (interactive)
  (when (and (not (active-minibuffer-window))
             (org-extra-buffer-is-note-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (org-extra-contains-tasks-p)
            (setq tags (cons "tasks" tags))
          (setq tags (remove "tasks" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'vulpea-buffer-tags-set tags))))))

(defun org-extra-buffer-is-note-p ()
  "Return non-nil if the currently visited buffer is a note."
  (interactive)
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))
