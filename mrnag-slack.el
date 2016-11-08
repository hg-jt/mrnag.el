;;; mrnag-slack.el --- A mrnag publisher reporting to a Slack incoming webhook.

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

;; This package provides a Slack publisher for mrnag.

;;; Code:
(require 'json)

(defcustom mrnag-slack-webhook-url nil
  "The Slack incoming webhook to post messages to.
to.

For example: https://hooks.slack.com/services/..."
  :group 'mrnag
  :type 'string)

(defcustom mrnag-slack-channel-override nil
  "An alternative to the webhooks default channel.

See https://api.slack.com/incoming-webhooks"
  :group 'mrnag
  :type 'string)

(defcustom mrnag-slack-message
  "<!here>: There are %d open merge requests in %d projects"
  "An informative message about the open merge requests.

This can be a format string that accepts up to two arguments: the
number of open merge requests and the total number of projects
that have open merge requests.

This string will ultimately get parsed by Slack's formatter as
well, Slack's rich text can also be used here."
  :group 'mrnag
  :type 'string)

;;;###autoload
(defun mrnag-slack (projects)
  "Publish report to Slack.

PROJECTS is an alist with elements are of the form (name . id)."
  (let* ((total (apply '+ (mapcar (lambda (mr)
                                    (length (assoc-default 'merge-requests mr)))
                                  projects)))
         (slack-request
          `(("text" . ,(format mrnag-slack-message total (length projects)))
            ("attachments" . ,(mapcar 'mrnag--slack-format-project projects))))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string (json-encode
                                 (if mrnag-slack-channel-override
                                     (push `("channel" . ,mrnag-slack-channel-override) slack-request)
                                   slack-request))
                                'utf-8)))
    ;(with-temp-file "~/code/slack.json" (insert url-request-data))
    (url-retrieve-synchronously mrnag-slack-webhook-url) ))

(defun mrnag--slack-format-project (project-alist)
  "Create a Slack attachment for the given PROJECT-ALIST."
  (let* ((project-id (assoc-default 'id project-alist))
         (project-name (assoc-default 'name project-alist))
         (project-merge-requests (mapcar 'identity (assoc-default 'merge-requests project-alist))))
    `(("title" . ,project-name)
      ("title_link" . ,(assoc-default 'project-url project-alist))
      ("fallback" . ,(format "%s (%d open merge requests)" project-name (length project-merge-requests)))
      ("mrkdwn_in" . ("text" "fields"))
      ;("thumb_url" . ,(assoc-default 'avatar-url project-alist))  ;; needs to be publically viewable
      ;("color" . ,(format "#%X%X" project-id project-id))
      ("fields" . ,(mapcar 'mrnag--slack-mr-to-field project-merge-requests))) ))

(defun mrnag--slack-mr-to-field (mr)
  "Convert an MR to a Slack attachment \"field\"."
  (let ((author (assoc-default 'author mr)))
    `(("title" . ,(replace-regexp-in-string "\*" "" (assoc-default 'title mr)))
      ("value" . ,(format "%s %s\n<%s|%d upvotes, %d comments>"
                          (assoc-default 'username author)
                          (or (mrnag--slack-gitlab-labels-to-emojiis (assoc-default 'labels mr)) "")
                          (assoc-default 'web_url mr)
                          (assoc-default 'upvotes mr)
                          (assoc-default 'user_notes_count mr)))
      ("short" . ,json-false))))

(defun mrnag--slack-gitlab-labels-to-emojiis (labels)
  "Convert GitLab LABELS to Slack emojiis."
  (when (> (length labels) 0)
    (mapconcat (lambda (label)
                 (format ":%s:" (downcase (replace-regexp-in-string " " "" label))))
               labels " ")))

(provide 'mrnag-slack)
;;; mrnag-slack.el ends here
