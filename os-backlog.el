;;; os-backlog.el --- Backlog (backlog.jp) backend for org-sync

;; Copyright (C) 2012  Aurelien Aptel
;; Copyright (C) 2016  Yoshiyuki Kinjo
;;
;; Author: Aurelien Aptel <aurelien dot aptel at gmail dot com>
;; Author: Yoshiyuki Kinjo <yskkin at gmail dot com>
;; Keywords: org, backlog, synchronization
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; This file is not part of GNU Emacs.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a backend for org-sync to synchnonize
;; issues from a backlog project with an org-mode buffer.  Read Org-sync
;; documentation for more information about it.

;;; Code:

(eval-when-compile (require 'cl))
(require 'os)
(require 'url)
(require 'json)

(defgroup os-backlog nil "org-mode and Backlog integration.")

(add-to-list 'os-backend-alist '("backlog.jp" . os-backlog-backend))
(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defvar os-backlog-backend
  '((base-url      . os-backlog-base-url)
    (fetch-buglist . os-backlog-fetch-buglist)
    (send-buglist  . os-backlog-send-buglist))
  "Backlog backend.")

(defcustom os-backlog-auth nil
  "Backlog api key."
  :type 'string
  :group 'os-backlog)

(defvar os-backlog-project-id nil
  "Project id of current buglist.")

(defconst os-backlog-date-regex
  (rx
   (seq
    (group (repeat 4 digit)) "/"
    (group (repeat 2 digit)) "/"
    (group (repeat 2 digit))
    "T"
    (group
     (repeat 2 digit) ":"
     (repeat 2 digit) ":"
     (repeat 2 digit))
    "Z"))
  "Regex to parse date returned by Backlog.")

(defun os-backlog-fetch-meta ()
  "Set `os-backlog-project-id' for now."
  (let* ((res (os-backlog-request "GET" (concat os-base-url ".json")))
         (code (car res))
         (json (cdr res)))
    (when (/= code 200)
      (error "Can't fetch data from %s, wrong url?" os-base-url))
    (setq os-backlog-project-id (cdr (assoc 'id (cdr (assoc 'project json)))))))

(defun os-backlog-parse-date (date)
  "Return time object of DATE."
  (when (string-match os-backlog-date-regex date)
    (os-parse-date (concat (match-string 1 date) "-"
                           (match-string 2 date) "-"
                           (match-string 3 date) "T"
                           (match-string 4 date)
                           (match-string 5 date)))))

(defun os-backlog-request (method url &optional data)
  "Send HTTP request at URL using METHOD with DATA.
Return the server decoded response in JSON."
  (let* ((url-request-method method)
         (url-request-data data)
         (url-request-extra-headers
          (when data
            '(("Content-Type" . "application/json"))))
         (auth os-backlog-auth)
         (buf))

    (when (stringp auth)
      (setq url (os-url-param url `(("apiKey" . ,auth)))))

    (message "%s %s %s" method url (prin1-to-string data))
    (setq buf (url-retrieve-synchronously url))
    (with-current-buffer buf
      (goto-char url-http-end-of-headers)
      (prog1
          (cons url-http-response-status (ignore-errors (json-read-from-string (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8))))
        (kill-buffer)))))

;; override
;;;###autoload
(defun os-backlog-base-url (url)
  "Return base URL."
  ;; if no url type, try https
  (when (not (string-match "^https?://" url))
    (setq url (concat "https://" url)))

  (let ((purl (url-generic-parse-url url)))
      (concat (url-type purl) "://"
              (url-host purl)
              "/api/v2")))

(defun os-backlog-project-name (url)
  "Return repo name at URL."
  (when (string-match "projects/\\([^/]+\\)" url)
    (match-string 1 url)))

(defun os-backlog-json-to-bug (json)
  "Return JSON as a bug."
  (cl-flet ((va (key alist) (cdr (assoc key alist)))
         (v (key) (va key json)))
    (let* ((id (v 'issueKey))
           (author (va 'name (v 'createdUser)))
           (txtstatus (va 'name (v 'status)))
           (status (if (string= txtstatus "完了")
                       'closed
                     'open))
           (priority (va 'name (v 'priority)))
           (title (v 'summary))
           (desc (v 'description))
           (ctime (os-backlog-parse-date (v 'created)))
           (mtime (os-backlog-parse-date (v 'updated))))

      `(:id ,id
            :priority ,priority
            :status ,status
            :title ,title
            :desc ,desc
            :date-creation ,ctime
            :date-modification ,mtime))))

;;;###autoload
(defun os-backlog-fetch-buglist (last-update)
  "Return the buglist at os-base-url."
  (let* ((url (concat os-base-url "/issues"))
         (res (os-backlog-request "GET" url))
         (code (car res))
         (json (cdr res))
         (title (concat "Issues of " (os-backlog-project-name url))))

    `(:title ,title
             :url ,os-base-url
             :bugs ,(mapcar 'os-backlog-json-to-bug json))))

(defun os-backlog-bug-to-json (bug)
  (json-encode
   `((issue .
            ((subject     . ,(os-get-prop :title bug))
             (description . ,(os-get-prop :desc bug)))))))

;;;###autoload
(defun os-backlog-send-buglist (buglist)
    "Send a BUGLIST on the bugtracker and return new bugs."
    (let* ((new-url (concat os-base-url "/issues.json"))
           (root-url (replace-regexp-in-string "/projects/.+"
                                               "" os-base-url))
           new-bugs)

      (os-backlog-fetch-meta)

      (dolist (b (os-get-prop :bugs buglist))
        (let* ((id (os-get-prop :id b))
               (data (os-backlog-bug-to-json b))
               (modif-url (format "%s/issues/%d.json" root-url (or id 0)))
               res)
          (cond
           ;; new bug
           ((null id)
            (setq res (os-backlog-request "POST" new-url data))
            (when (/= (car res) 201)
              (error "Can't create new bug \"%s\"" (os-get-prop :title b)))
            (push (os-backlog-json-to-bug
                   (cdr (assoc 'issue (cdr res))))
                  new-bugs))

           ;; delete bug
           ((os-get-prop :delete b)
            (setq res (os-backlog-request "DELETE" modif-url))
            (when (not (member (car res) '(404 204)))
              (error "Can't delete bug #%d" id)))

           ;; update bug
           (t
            (setq res (os-backlog-request "PUT" modif-url data))
            (when (/= (car res) 200)
              (error "Can't update bug #%d" id))

            ;; fetch the new version since redmine doesn't send it
            (setq res (os-backlog-request "GET" modif-url))
            (when (/= (car res) 200)
              (error "Can't update bug #%d" id))

            (push (os-backlog-json-to-bug
                   (cdr (assoc 'issue (cdr res))))
                  new-bugs)))))
      `(:bugs ,new-bugs)))

(provide 'os-backlog)
;;; os-backlog.el ends here
