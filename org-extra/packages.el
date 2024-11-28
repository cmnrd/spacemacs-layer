;; Copyright (c) 2021 Christian Menard
;;
;; Author: Christian Menard <christian.menard@gmx.de>
;;
;; This file is not part of GNU Emacs.

(defconst org-extra-packages
  '(vulpea
    org
    crux
    org-roam-bibtex
    org-fancy-priorities
    org-ql))

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
  ;; Automatically switch to insert mode when capturing
  (add-hook 'org-capture-mode-hook 'evil-insert-state 90))

(defun org-extra/init-org-ql ()
  (use-package org-ql
    :ensure t
    :config
    (require 'org-ql-search)
    ;; Extends org-ql dynamic blocks by the :scope argument
    ;; Use org-extra-ql :scope ... instead of org-ql
    ;; source https://github.com/alphapapa/org-ql/pull/239#issuecomment-1213052966
    (cl-defun org-dblock-write:org-extra-ql (params)
      "Insert content for org-ql dynamic block at point according to PARAMS.
Valid parameters include:
 :scope    The scope to consider for the Org QL query. This can
            be one of the following:
            `buffer'              the current buffer
            `org-agenda-files'    all agenda files
            `org-directory'       all org files
            `(\"path\" ...)'      list of buffer names or file paths
            `all'                 all agenda files, and org-mode buffers

  :query    An Org QL query expression in either sexp or string
            form.

  :columns  A list of columns, including `heading', `todo',
            `property',`priority',`deadline',`scheduled',`closed'.
            Each column may also be specified as a list with the
            second element being a header string.  For example,
            to abbreviate the priority column: (priority \"P\").
            For certain columns, like `property', arguments may
            be passed by specifying the column type itself as a
            list.  For example, to display a column showing the
            values of a property named \"milestone\", with the
            header being abbreviated to \"M\":

              ((property \"milestone\") \"M\").

  :sort     One or a list of Org QL sorting methods
            (see `org-ql-select').

  :take     Optionally take a number of results from the front (a
            positive number) or the end (a negative number) of
            the results.

  :ts-format  Optional format string used to format
              timestamp-based columns.

For example, an org-ql dynamic block header could look like:

  #+BEGIN: org-ql :query (todo \"UNDERWAY\") :columns (priority todo heading) :sort (priority date) :ts-format \"%Y-%m-%d %H:%M\""
      (-let* (((&plist :scope :query :columns :sort :ts-format :take) params)
              (org-extra-update-agenda-files)
              (query (cl-etypecase query
                       (string (org-ql--query-string-to-sexp query))
                       (list  ;; SAFETY: Query is in sexp form: ask for confirmation, because it could contain arbitrary code.
                        (org-ql--ask-unsafe-query query)
                        query)))
              (columns (or columns '(heading todo (priority "P"))))
              (scope (cond ((and (listp scope) (seq-every-p #'stringp scope)) scope)
                           ((string-equal scope "org-agenda-files") (org-agenda-files))
                           ((or (not scope) (string-equal scope "buffer")) (current-buffer))
                           ((string-equal scope "org-directory") (org-ql-search-directories-files))
                           (t (user-error "Unknown scope '%s'" scope))))
              ;; MAYBE: Custom column functions.
              (format-fns
               ;; NOTE: Backquoting this alist prevents the lambdas from seeing
               ;; the variable `ts-format', so we use `list' and `cons'.
               (list (cons 'todo (lambda (element)
                                   (org-element-property :todo-keyword element)))
                     (cons 'heading (lambda (element)
                                      (cond
                                       ((and org-id-link-to-org-use-id
                                             (org-element-property :ID element))
                                        (org-make-link-string (format "id:%s" (org-element-property :ID element))
                                                              (org-element-property :raw-value element)))
                                       ((org-element-property :file element)
                                        (org-make-link-string (format "file:%s::*%s"
                                                                      (org-element-property :file element)
                                                                      (org-element-property :raw-value element))
                                                              (org-element-property :raw-value element)))
                                       (t (org-make-link-string (org-element-property :raw-value element)
                                                                (org-link-display-format
                                                                 (org-element-property :raw-value element)))))
                                      ))
                     (cons 'priority (lambda (element)
                                       (--when-let (org-element-property :priority element)
                                         (char-to-string it))))
                     (cons 'deadline (lambda (element)
                                       (--when-let (org-element-property :deadline element)
                                         (ts-format ts-format (ts-parse-org-element it)))))
                     (cons 'scheduled (lambda (element)
                                        (--when-let (org-element-property :scheduled element)
                                          (ts-format ts-format (ts-parse-org-element it)))))
                     (cons 'closed (lambda (element)
                                     (--when-let (org-element-property :closed element)
                                       (ts-format ts-format (ts-parse-org-element it)))))
                     (cons 'property (lambda (element property)
                                       (org-element-property (intern (concat ":" (upcase property))) element)))))
              (elements (org-ql-query :from scope
                                      :where query
                                      :select '(org-element-put-property (org-element-headline-parser (line-end-position)) :file (buffer-file-name))
                                      :order-by sort)))
        (when take
          (setf elements (cl-etypecase take
                           ((and integer (satisfies cl-minusp)) (-take-last (abs take) elements))
                           (integer (-take take elements)))))
        (cl-labels ((format-element
                      (element) (string-join (cl-loop for column in columns
                                                      collect (or (pcase-exhaustive column
                                                                    ((pred symbolp)
                                                                     (funcall (alist-get column format-fns) element))
                                                                    (`((,column . ,args) ,_header)
                                                                     (apply (alist-get column format-fns) element args))
                                                                    (`(,column ,_header)
                                                                     (funcall (alist-get column format-fns) element)))
                                                                  ""))
                                             " | ")))
          ;; Table header
          (insert "| " (string-join (--map (pcase it
                                             ((pred symbolp) (capitalize (symbol-name it)))
                                             (`(,_ ,name) name))
                                           columns)
                                    " | ")
                  " |" "\n")
          (insert "|- \n")  ; Separator hline
          (dolist (element elements)
            (insert "| " (format-element element) " |" "\n"))
          (delete-char -1)
          (org-table-align))))))
