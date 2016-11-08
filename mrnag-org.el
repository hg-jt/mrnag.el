;;; mrnag-org.el --- A mrnag publisher reporting in an org-mode buffer.

;; Copyright (C) 2016 hg-jt
;; Author: hg-jt <hg-jt@users.noreply.github.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a org-mode publisher for mrnag.

;;; Code:
(require 'org)
(require 'org-element)

;;;###autoload
(defun mrnag-org (projects)
  "Publish report to an `org-mode' buffer.

The argument PROJECTS should be an alist with at least three
elements: ID, NAME, and MERGE_REQUESTS.

ID can be the id or namespace/projectname of the project or the
project's numeric GitLab id.

NAME is a displayable string that may be used for publishing
reports.

MERGE-REQUESTS is a vector of open merge requests associated with
the given project. Each item in the vector is an alist
representation of a merge request from the GitLab API."
  (let ((total (apply '+ (mapcar (lambda (mr)
                                  (length (assoc-default 'merge-requests mr)))
                                 projects))))
    (with-current-buffer (switch-to-buffer (format-time-string "*mrnag-%Y%m%d*" (current-time)))
      (erase-buffer)
      (insert "#+TITLE: Open Merge Requests\n")
      (insert "#+TODO: TODO(t) REVIEW(r) | DONE(d)")
      (org-mode)
      (insert (format "\nThere are %d open merge requests in %d projects:\n\n"
                      total
                      (length projects) ))
      (mapc 'mrnag--org-format-project projects))))

(defun mrnag--org-format-project (project-alist)
  "Create an org entry for a PROJECT."
  (let ((project-name (assoc-default 'name project-alist))
        (project-merge-requests (mapcar 'identity (assoc-default 'merge-requests project-alist))))
    (when (and project-merge-requests (> (length project-merge-requests) 0))
      (org-insert-heading t nil t)
      (insert (format "%s\n\n" project-name))
      ;; the first one represents an new subheading
      (mrnag--org-format-mr (car project-merge-requests) t)
      (mapc 'mrnag--org-format-mr (cdr project-merge-requests)))))

(defun mrnag--org-format-mr (mr &optional first-subheading)
  "Create an org entry for a merge request.
Argument MR is a parsed JSON document represented as an alist.

Optional argument FIRST-SUBHEADING is a flag indicating if a new
sub-heading should be created."
  (if first-subheading
      (org-insert-todo-subheading nil)
    (org-insert-todo-heading nil))
  (let ((labels (assoc-default 'labels mr)))
    (org-insert-link nil (assoc-default 'web_url mr) (assoc-default 'title mr))
    (when (> (length labels) 0)
      (insert (format " %s" (mrnag--org-gitlab-labels-to-tags labels))))
    (insert "\n")
    (save-excursion
      (when (ignore-errors (org-back-to-heading))
        (org-set-tags nil t)
        (unless (zerop (+ (or (assoc-default 'upvotes mr) 0)
                          (or (assoc-default 'user_notes_count mr) 0)))
          (org-todo 2))
        (org-update-parent-todo-statistics)
        (org-update-checkbox-count))))
  (org-set-property "upvotes" (number-to-string (assoc-default 'upvotes mr)))
  (org-set-property "comments" (number-to-string (assoc-default 'user_notes_count mr)))
  (org-set-property "author" (assoc-default 'name (assoc-default 'author mr)))
  (org-set-property "created_at"
                    (format-time-string "%m/%d/%Y %H:%M %Z"
                                        (date-to-time (assoc-default 'created_at mr)))) )

(defun mrnag--org-gitlab-labels-to-tags (labels)
  "Convert GitLab LABELS to `org-mode' tags."
  (when (> (length labels) 0)
    (downcase (replace-regexp-in-string " " ":" (concat ":" (mapconcat 'identity labels ":") ":")))))

(provide 'mrnag-org)
;;; mrnag-org.el ends here
