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
;;   M-x package-install-file <path-to-mrnag>
;;
;;
;; Configuration:
;;   (eval-after-load "mrnag"
;;     (lambda ()
;;       (setq mrnag-gitlab-baseurl "https://gitlab.example.com/api/v3"
;;             mrnag-gitlab-token   "xxxxxxxxxxxxxxxxxxxx"
;;             mrnag-projects-alist '((foo . 123)
;;                                    (bar . 456)))))
;;
;; Usage:
;;   M-x mrnag

;;; Change Log:

;; 2016-07-22  hg-jt  <hg-jt@users.noreply.github.com>
;;
;;  * Initial project.
;;
;; 2016-10-12  hg-jt  <hg-jt@users.noreply.github.com>
;;
;;  * Extracted org-mode publisher to separate file.
;;  * Added Slack publisher (mrnag-slack.el)

;;; Code:
(require 'json)
(require 'url)

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
  :type '(alist :key-type 'string :value-type 'number))

(defcustom mrnag-gitlab-enable-mr-cache nil
  "Enables a cache for GitLab merge requests API calls.

If non-nill will enable a cache to limit the number of requests going out to the
GitLab API.  This is useful for testing and disabled by default."
  :group 'mrnag
  :type 'boolean)

(defcustom mrnag-publishers-alist '((org . mrnag-org)
                                    (slack . mrnag-slack))
  "An alist of mrnag publishers."
  :group 'mrnag
  :type '(alist :key-type 'symbol :value-type 'function))

(defcustom mrnag-publisher 'org
  "The selected publisher."
  :group 'mrnag
  :type 'symbol)

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

(defun mrnag-gitlab-projects (project-id)
  "Queries GitLab for the given PROJECT-ID.

PROJECT_ID can be the id or namespace/projectname of a project."
  (let ((gitlab-projects-url (concat mrnag-gitlab-baseurl (format "/projects/%s" project-id)))
        (url-request-method "GET")
        (url-request-extra-headers `(,(cons "PRIVATE-TOKEN" mrnag-gitlab-token)) ))
    (with-current-buffer
        (url-retrieve-synchronously gitlab-projects-url)
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
                          (let ((project-details (mrnag-gitlab-projects project-id)))
                            `((id . ,project-id)
                              (name . ,project-name)
                              (merge-requests . ,project-mrs)
                              (project-url . ,(assoc-default 'web_url project-details))
                              (avatar-url  . ,(assoc-default 'avatar_url project-details))
                              ) ))))
                    mrnag-projects-alist)))

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
    (funcall publisher merge-requests-alist) ))

(provide 'mrnag)
;;; mrnag.el ends here
