;;; org-asana-sync.el --- Two-way sync between Org Mode and Asana -*- lexical-binding: t; -*-

;; Copyright (C) 2025  William Theesfeld

;; Author: William Theesfeld <william@theesfeld.net>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: org, comm, tools
;; URL: https://github.com/theesfeld/org-asana-sync

;;; Commentary:

;; This package provides two-way synchronization between Emacs Org Mode and Asana.
;; It maps Asana tasks to Org headlines using property drawers to store metadata
;; (ASANA_ID, ASANA_LINK, etc.).

;;; Code:

(require 'org)
(require 'json)
(require 'url)
(require 'auth-source)
(require 'map)
(require 'seq)
(require 'cl-lib)

(defgroup org-asana-sync nil
  "Two-way sync between Org Mode and Asana."
  :group 'org
  :prefix "org-asana-sync-")

(defcustom org-asana-sync-workspace-id nil
  "The Asana Workspace GID to sync with.
If nil, the first available workspace will be used."
  :type '(choice (const :tag "Auto-detect" nil)
                 (string :tag "Workspace ID"))
  :group 'org-asana-sync)

(defcustom org-asana-sync-file nil
  "The specific Org file to sync Asana tasks into.
If nil, tasks will be synced to the current buffer."
  :type '(choice (const :tag "Current Buffer" nil)
                 (file :tag "Specific File"))
  :group 'org-asana-sync)

(defcustom org-asana-sync-todo-keyword "TODO"
  "Org keyword for incomplete Asana tasks."
  :type 'string
  :group 'org-asana-sync)

(defcustom org-asana-sync-done-keyword "DONE"
  "Org keyword for completed Asana tasks."
  :type 'string
  :group 'org-asana-sync)

;;; Auth & API

(defun org-asana-sync--get-token ()
  "Retrieve the Asana Personal Access Token from auth-sources."
  (let ((found (auth-source-search :host "app.asana.com" :user "apikey")))
    (if found
        (funcall (plist-get (car found) :secret))
      (error "Asana API key not found in auth-sources (host: app.asana.com, user: apikey)"))))

(defconst org-asana-sync--api-base "https://app.asana.com/api/1.0")

(defun org-asana-sync--request (method endpoint &optional data)
  "Send a request to the Asana API."
  (let ((url-request-method method)
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " (org-asana-sync--get-token)))
           ("Content-Type" . "application/json")))
        (url-request-data (when data (json-encode `((data . ,data)))))
        (url (concat org-asana-sync--api-base endpoint))
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (let ((response (json-read)))
        (kill-buffer)
        (if (map-elt response "errors")
            (error "Asana API Error: %s" (map-elt response "errors"))
          (map-elt response "data"))))))

;;; Helpers

(defun org-asana-sync--find-entry-by-id (id)
  "Find the marker for an entry with ASANA_ID property matching ID."
  (let ((match (org-map-entries 'point (format "ASANA_ID=\"%s\"" id) nil)))
    (when match
      (car match))))

(defun org-asana-sync--map-status-to-org (completed)
  "Return the appropriate Org keyword based on COMPLETED status."
  (if (eq completed t) org-asana-sync-done-keyword org-asana-sync-todo-keyword))

(defun org-asana-sync--map-status-from-org (keyword)
  "Return boolean completed status based on Org KEYWORD."
  (if (member keyword org-done-keywords) t nil))

(defun org-asana-sync--update-headline-title (new-title)
  "Update the title of the headline at point to NEW-TITLE, preserving tags/todo."
  (org-back-to-heading t)
  (when (looking-at org-complex-heading-regexp)
    (let ((begin-title (match-beginning 4))
          (end-title (match-end 4)))
      (when (and begin-title end-title)
        (delete-region begin-title end-title)
        (goto-char begin-title)
        (insert new-title)))))

(defun org-asana-sync--update-entry-at-point (task)
  "Update the Org entry at point with data from Asana TASK."
  (let* ((name (map-elt task "name"))
         (completed (map-elt task "completed"))
         (permalink (map-elt task "permalink_url"))
         (due-on (map-elt task "due_on")))
    
    (org-asana-sync--update-headline-title name)
    (org-todo (org-asana-sync--map-status-to-org completed))
    
    (org-entry-put (point) "ASANA_LINK" permalink)
    (org-entry-put (point) "ASANA_LAST_SYNC" (format-time-string "%Y-%m-%d %H:%M:%S"))
    
    (if due-on
        (org-deadline nil due-on)
      (org-deadline nil nil))))

(defun org-asana-sync--create-entry (task)
  "Create a new Org entry for Asana TASK at the end of the file."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (let* ((gid (map-elt task "gid"))
         (name (map-elt task "name"))
         (completed (map-elt task "completed"))
         (notes (map-elt task "notes"))
         (permalink (map-elt task "permalink_url"))
         (due-on (map-elt task "due_on")))
    
    (insert (format "* %s %s\n" 
                    (org-asana-sync--map-status-to-org completed)
                    name))
    (let ((entry-pos (point)))
      (org-entry-put (point) "ASANA_ID" gid)
      (org-entry-put (point) "ASANA_LINK" permalink)
      (org-entry-put (point) "ASANA_LAST_SYNC" (format-time-string "%Y-%m-%d %H:%M:%S"))
      (when due-on
        (org-deadline nil due-on))
      (when (and notes (not (string-empty-p notes)))
        (insert (format "\n%s\n" notes))))))

;;; Main Commands

;;;###autoload
(defun org-asana-sync-pull ()
  "Pull tasks from Asana and update/create Org entries."
  (interactive)
  (let ((buf (if org-asana-sync-file
                 (find-file-noselect org-asana-sync-file)
               (current-buffer))))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (error "Buffer %s is not in Org mode" (buffer-name)))
      
      (message "Fetching user info...")
      (let* ((user-info (org-asana-sync--request "GET" "/users/me"))
             (user-gid (map-elt user-info "gid"))
             (workspace (or org-asana-sync-workspace-id
                            (map-elt (car (map-elt user-info "workspaces")) "gid"))))
        
        (message "Fetching tasks from Asana workspace %s..." workspace)
        (let ((tasks (org-asana-sync--request 
                      "GET" 
                      (format "/tasks?assignee=%s&workspace=%s&opt_fields=name,notes,completed,due_on,permalink_url"
                              user-gid workspace))))
          
          (dolist (task tasks)
            (let* ((gid (map-elt task "gid"))
                   (pos (org-asana-sync--find-entry-by-id gid)))
              (if pos
                  (save-excursion
                    (goto-char pos)
                    (org-asana-sync--update-entry-at-point task)
                    (message "Updated: %s" (map-elt task "name")))
                (org-asana-sync--create-entry task)
                (message "Created: %s" (map-elt task "name")))))
          (save-buffer)
          (message "Asana Sync Complete."))))))

;;;###autoload
(defun org-asana-sync-push-at-point ()
  "Push the current Org entry to Asana (Update or Create)."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (error "Not in Org mode"))
  
  (let* ((props (org-entry-properties))
         (gid (cdr (assoc "ASANA_ID" props)))
         (title (org-get-heading t t t t))
         (status (org-get-todo-state))
         (completed (org-asana-sync--map-status-from-org status))
         ;; Asana requires explicit boolean or null
         (completed-val (if completed t :json-false))
         (data `((name . ,title)
                 (completed . ,completed-val))))
    
    (if gid
        (progn
          (message "Updating task %s in Asana..." gid)
          (org-asana-sync--request "PUT" (format "/tasks/%s" gid) data)
          (org-entry-put (point) "ASANA_LAST_SYNC" (format-time-string "%Y-%m-%d %H:%M:%S"))
          (message "Task updated."))
      ;; Create new task
      (message "Creating new task in Asana...")
      (let* ((user-info (org-asana-sync--request "GET" "/users/me"))
             (user-gid (map-elt user-info "gid"))
             (workspace (or org-asana-sync-workspace-id
                            (map-elt (car (map-elt user-info "workspaces")) "gid")))
             (create-data (append data
                                  `((assignee . ,user-gid)
                                    (workspace . ,workspace)))))
        (let ((response (org-asana-sync--request "POST" "/tasks" create-data)))
          (org-entry-put (point) "ASANA_ID" (map-elt response "gid"))
          (org-entry-put (point) "ASANA_LINK" (map-elt response "permalink_url"))
          (org-entry-put (point) "ASANA_LAST_SYNC" (format-time-string "%Y-%m-%d %H:%M:%S"))
          (message "Task created in Asana: %s" (map-elt response "gid")))))))

(provide 'org-asana-sync)
;;; org-asana-sync.el ends here