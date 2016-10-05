;;; mrnag.el --- A GitLab merge request aggregator and publisher.

;; Copyright (C) 2016 hg-jt
;; Author: hg-jt <hg-jt@users.noreply.github.com>
;; Version: 0.1
;; Keywords: gitlab

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

;; This package provides utilities for aggregating and publishing
;; information about open merge requests in one or more GitLab
;; projects.
;;
;; mrnag does not have any external package dependencies.
;;
;; Installation:
;;   M-x package-install-file <path-to-mrnag>/mrnag.el
;;
;;
;; Configuration:
;;   (eval-after-load "mrnag"
;;     '(setq mrnag-gitlab-baseurl "https://gitlab.example.com/api/v3"
;;            mrnag-gitlab-token   "xxxxxxxxxxxxxxxxxxxx"
;;            mrnag-projects-alist '((foo . 123)
;;                                   (bar . 456))))
;;
;; Usage:
;;   M-x mrnag

;;; Change Log:

;; 2016-07-22  hg-jt  <hg-jt@users.noreply.github.com>
;;
;;  * Initial project.

;;; Code:
(require 'json)
(require 'url)
(require 'org)
(require 'org-element)

(defgroup mrnag nil
  "GitLab merge request reporter."
  :group 'tools)

(defcustom mrnag-gitlab-baseurl nil
  "The base url to use for GitLab API requests (e.g. https://gitlab.example.com/api/v3)."
  :group 'mrnag
  :type 'string)

(defcustom mrnag-gitlab-token nil
  "The private token to use for GitLab API requests."
  :group 'mrnag
  :type 'string)

(defcustom mrnag-projects-alist nil
  "An association list of project names and ids.

    ((foo . 123))
     (bar . 456))"
  :group 'mrnag
  :type '(alist :key-type 'symbol :value-type 'number))

(defcustom mrnag-gitlab-enable-mr-cache nil
  "Enables a cache for GitLab merge requests API calls.

If non-nill will enable a cache to limit the number of requests going out to the
GitLab API.  This is useful for testing and disabled by default."
  :group 'mrnag
  :type 'boolean)

(defcustom mrnag-publishers-alist '((org . mrnag-publish-to-org))
  "An alist of mrnag publishers."
  :group 'mrnag
  :type '(alist :key-type 'symbol :value-type 'function))

(defcustom mrnag-publisher 'org
  "The selected publisher."
  :group 'mrnag
  :type 'symbol)

(defcustom mrnag-publisher-args nil
  "Arguments to send to the selected publisher."
  :group 'mrnag
  :type 'list)

(defvar mrnag--gitlab-mr-cache nil
  "A cached version of the aggregated list of merge requests.")


(defun mrnag-gitlab-merge-requests (project-id &optional state)
  "Queries GitLab for merge requests associated with a given PROJECT-ID.

PROJECT_ID can be the id or namespace/projectname of a project.
The results will be filtered to include only merge requests with
the given STATE or all merge requests if STATE is nil.

Returns a vector of association lists each of which represents a merge request."
  (let ((gitlab-mergerequests-url (concat mrnag-gitlab-baseurl
                                          (format "/projects/%s/merge_requests" project-id)))
        (url-request-method "GET")
        (url-request-extra-headers `(,(cons "PRIVATE-TOKEN" mrnag-gitlab-token)) ))
      (with-current-buffer
          (url-retrieve-synchronously (if state
                                          (concat gitlab-mergerequests-url (format "?state=%s" state))
                                        gitlab-mergerequests-url))
        (goto-char (point-min))
        (re-search-forward "^$")            ; find the end of the headers
        (delete-region (point) (point-min)) ; delete the headers
        (json-read-from-string (buffer-string))) ))

(defun mrnag-gitlab-aggregate-merge-requests ()
  "Create an aggregated list of merge requests.

The result will be an alist with the form (project-id . [merge-requests]).
Projects without any open merge requests will be filtered out of the result."
  (delq nil (mapcar (lambda (project)
                      (let* ((project-name (car project))
                             (project-id (cdr project))
                             (project-mrs (mrnag-gitlab-merge-requests project-id "opened")))
                        (when (> (length project-mrs) 0)
                          `((id . ,project-id)
                            (name . ,project-name)
                            (merge-requests . ,project-mrs)) )))
                          ;(cons project-id merge-requests))))
                    mrnag-projects-alist)))

(defun mrnag-publish-to-org (projects &rest args)
  "Publish report to an `org-mode' buffer.

The argument PROJECTS should be an alist with three elements: ID,
NAME, and MERGE_REQUESTS.

ID can be the id or namespace/projectname of the project or the
project's numeric GitLab id.

NAME is a displayable string that may be used for publishing
reports.

MERGE-REQUESTS is a vector of open merge requests associated with
the given project. Each item in the vector is an alist
representation of a merge request from the GitLab API.

Optional argument ARGS contains any additional parameters for the
publisher (not used by the org publisher)."
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
      (insert (format "%s\n\n" (symbol-name project-name)))
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
      (insert (format " %s" (mrnag-gitlab-labels-to-org-tags labels))))
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

(defun mrnag-gitlab-labels-to-org-tags (labels)
  "Convert GitLab LABELS to `org-mode' tags."
  (when (> (length labels) 0)
    (downcase (replace-regexp-in-string " " ":" (concat ":" (mapconcat 'identity labels ":") ":")))))

;;;###autoload
(defun mrnag ()
  "Aggregates merge request data from GitLab and formats it into a report.

The default formatter creates an Org buffer with the data."
  (interactive)
  (let ((merge-requests-alist
         (if mrnag-gitlab-enable-mr-cache
             (progn
               (when (not mrnag--gitlab-mr-cache)
                 (setq mrnag--gitlab-mr-cache (mrnag-gitlab-aggregate-merge-requests)))
               mrnag--gitlab-mr-cache)
           (mrnag-gitlab-aggregate-merge-requests)))
        (publisher (assoc-default mrnag-publisher mrnag-publishers-alist)))
    (funcall publisher merge-requests-alist mrnag-publisher-args) ))

(provide 'mrnag)
;;; mrnag.el ends here
