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
;; (ASANA_ID, ASANA_LINK, etc.). It also allows for team-based syncing and
;; dynamic generation of Org tags and agenda views based on Asana team members.

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

(defcustom org-asana-sync-team-gid nil
  "The Asana Team GID to sync tasks from.
If nil, only tasks assigned to the current user in their primary workspace will be synced.
Set this to a Team GID (e.g., \"1234567890\") for team-wide sync."
  :type '(choice (const :tag "No specific team" nil)
                 (string :tag "Team GID"))
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

(defun org-asana-sync--request (method endpoint &optional data query-params)
  "Send a request to the Asana API.
QUERY-PARAMS is an alist of key-value pairs to be appended to the URL."
  (let ((url-request-method method)
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " (org-asana-sync--get-token)))
           ("Content-Type" . "application/json")))
        (url-request-data (when data (json-encode `((data . ,data)))))
        (full-url (concat org-asana-sync--api-base endpoint
                          (if query-params
                              (concat "?" (url-encode-query-string query-params))
                            "")))
        (json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (with-current-buffer (url-retrieve-synchronously full-url)
      (goto-char (point-min))
      (re-search-forward "$" nil 'move)
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

(defun org-asana-sync--assignee-name-to-tag (assignee-name)
  "Convert Asana ASSIGNEE-NAME to a valid Org tag string (e.g., 'Ryan Gosling' -> '@ryan_gosling')."
  (when assignee-name
    (concat "@" (replace-regexp-in-string "[^a-zA-Z0-9]+" "_" (downcase assignee-name)))))

(defun org-asana-sync--get-user-by-gid (gid team-members)
  "Find user name in TEAM-MEMBERS alist given GID."
  (when gid
    (map-elt (seq-find (lambda (member) (string= (map-elt member "gid") gid))
                       team-members)
             "name")))

(defun org-asana-sync--update-entry-at-point (task team-members)
  "Update the Org entry at point with data from Asana TASK.
TEAM-MEMBERS is an alist of fetched team members to resolve assignee names."
  (let* ((name (map-elt task "name"))
         (completed (map-elt task "completed"))
         (permalink (map-elt task "permalink_url"))
         (due-on (map-elt task "due_on"))
         (assignee-gid (map-elt (map-elt task "assignee") "gid"))
         (assignee-name (org-asana-sync--get-user-by-gid assignee-gid team-members))
         (assignee-tag (org-asana-sync--assignee-name-to-tag assignee-name)))
    
    (org-asana-sync--update-headline-title name)
    (org-todo (org-asana-sync--map-status-to-org completed))
    
    (org-entry-put (point) "ASANA_LINK" permalink)
    (org-entry-put (point) "ASANA_LAST_SYNC" (format-time-string "%Y-%m-%d %H:%M:%S"))
    
    (if due-on
        (org-deadline nil due-on)
      (org-deadline nil nil))
    
    ;; Add assignee tag
    (when assignee-tag
      (org-set-tags (cl-union (org-get-tags t) (list assignee-tag) :test 'string=) t)))

(defun org-asana-sync--create-entry (task team-members)
  "Create a new Org entry for Asana TASK at the end of the file.
TEAM-MEMBERS is an alist of fetched team members to resolve assignee names."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (let* ((gid (map-elt task "gid"))
         (name (map-elt task "name"))
         (completed (map-elt task "completed"))
         (notes (map-elt task "notes"))
         (permalink (map-elt task "permalink_url"))
         (due-on (map-elt task "due_on"))
         (assignee-gid (map-elt (map-elt task "assignee") "gid"))
         (assignee-name (org-asana-sync--get-user-by-gid assignee-gid team-members))
         (assignee-tag (org-asana-sync--assignee-name-to-tag assignee-name))
         (tags (when assignee-tag (list assignee-tag))))
    
    (insert (format "* %s %s%s\n"
                    (org-asana-sync--map-status-to-org completed)
                    name
                    (if tags (concat " :" (string-join tags ":") ":") "")))
    (let ((entry-pos (point))))
      (org-entry-put (point) "ASANA_ID" gid)
      (org-entry-put (point) "ASANA_LINK" permalink)
      (org-entry-put (point) "ASANA_LAST_SYNC" (format-time-string "%Y-%m-%d %H:%M:%S"))
      (when due-on
        (org-deadline nil due-on))
      (when (and notes (not (string-empty-p notes))))
        (insert (format "\n%s\n" notes)))))

;;; Dynamic Org Config Generation

(defconst org-asana-sync--colors
  '("RoyalBlue" "DarkGreen" "DarkCyan" "DarkOrange" "DarkViolet" "Chocolate"
    "Firebrick" "ForestGreen" "DeepSkyBlue" "Goldenrod" "#ff79c6" "#8be9fd" "#50fa7b" "#bd93f9")
  "A list of colors for dynamically assigned tags. Added existing theme colors to cycle.")

(defun org-asana-sync--generate-team-tag-alist (team-members)
  "Generate `org-tag-alist` entries for TEAM-MEMBERS."
  (cl-loop for member in team-members
           for name = (map-elt member "name")
           for tag = (org-asana-sync--assignee-name-to-tag name)
           if (and tag (not (assoc tag org-tag-alist))) ; Avoid overwriting existing custom keys
           collect (list tag (intern (format "?%c" (char-downcase (elt name 0)))))))

(defun org-asana-sync--generate-team-tag-faces (team-members)
  "Generate `org-tag-faces` entries for TEAM-MEMBERS."
  (cl-loop for member in team-members
           for name = (map-elt member "name")
           for tag = (org-asana-sync--assignee-name-to-tag name)
           for color in (cl-cycle-list org-asana-sync--colors) ; Cycle through colors
           if (and tag (not (assoc tag org-tag-faces))) ; Avoid overwriting
           collect (list tag `(:foreground ,color :weight bold))))

(defun org-asana-sync--generate-team-agenda-commands (team-members)
  "Generate `org-agenda-custom-commands` entries for TEAM-MEMBERS."
  (cl-loop for member in team-members
           for name = (map-elt member "name")
           for tag = (org-asana-sync--assignee-name-to-tag name)
           if tag
           collect (list (intern (cl-upper-case (substring tag 1 2))) ; Use first char of tag (capitalized) for shortcut
                         (format "%s's Tasks" name)
                         (list 'tags-todo (concat "+" tag)
                               `((org-agenda-overriding-header ,(format "%s's Tasks" name)))))))

;;;###autoload
(defun org-asana-sync-setup-team-config ()
  "Fetch Asana team members and dynamically configure Org tags and agenda views.
Requires `org-asana-sync-team-gid` to be set.
This function will append/replace generated configurations into the respective Org variables."
  (interactive)
  (unless org-asana-sync-team-gid
    (error "Please set `org-asana-sync-team-gid` first."))
  
  (message "Fetching team members from Asana...")
  (let* ((team-members (org-asana-sync--request "GET" (format "/teams/%s/users" org-asana-sync-team-gid) nil '(("opt_fields" . "gid,name")))))
    (if team-members
        (progn
          (message "Generating dynamic Org config for %d team members..." (length team-members))
          ;; Remove existing generated tags from org-tag-alist before appending new ones
          (setq org-tag-alist (cl-delete-if (lambda (item)
                                              (cl-find-if (lambda (m) (string= (map-elt m "name") (substring (car item) 1)))
                                                          team-members))
                                            org-tag-alist))
          (setq org-tag-alist (append org-tag-alist (org-asana-sync--generate-team-tag-alist team-members)))
          
          ;; Remove existing generated tag faces from org-tag-faces before appending new ones
          (setq org-tag-faces (cl-delete-if (lambda (item)
                                              (cl-find-if (lambda (m) (string= (map-elt m "name") (substring (car item) 1)))
                                                          team-members))
                                            org-tag-faces))
          (setq org-tag-faces (append org-tag-faces (org-asana-sync--generate-team-tag-faces team-members)))
          
          ;; Remove existing generated agenda commands for team dashboard
          (setq org-agenda-custom-commands
                (cl-delete-if (lambda (item) (and (listp item) (string= (car item) "T") (listp (elt item 2)) (string= (car (elt (elt item 2) 0)) "Team Dashboard")))
                              org-agenda-custom-commands))
          ;; Add new generated agenda commands
          (setq org-agenda-custom-commands (append org-agenda-custom-commands
                                                   `(("T" "Team Dashboard" ,(org-asana-sync--generate-team-agenda-commands team-members))))))
      (error "Failed to fetch team members. Check Asana GID and permissions."))))


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
                            (map-elt (car (map-elt user-info "workspaces")) "gid")))
             
             (team-members nil)
             (projects-in-team-gids nil)
             (assignee-gids nil)
             (tasks-query-params `(("opt_fields" . "name,notes,completed,due_on,permalink_url,assignee.gid,assignee.name")))
             (tasks nil))
        
        (when org-asana-sync-team-gid
          (message "Fetching team members for team %s..." org-asana-sync-team-gid)
          (setq team-members (org-asana-sync--request "GET" (format "/teams/%s/users" org-asana-sync-team-gid) nil '(("opt_fields" . "gid,name"))))
          (setq assignee-gids (mapcar (lambda (m) (map-elt m "gid")) team-members))
          
          (message "Fetching projects for team %s..." org-asana-sync-team-gid)
          (let ((projects (org-asana-sync--request "GET" (format "/teams/%s/projects" org-asana-sync-team-gid) nil '(("opt_fields" . "gid")))))
            (setq projects-in-team-gids (mapcar (lambda (p) (map-elt p "gid")) projects)))
          
          (setq tasks-query-params
                (nconc tasks-query-params
                       `(("assignee.any" . ,(string-join assignee-gids ","))
                         ("projects.any" . ,(string-join projects-in-team-gids ",")))))
          
          (message "Fetching tasks from Asana for team %s..." org-asana-sync-team-gid)
          (setq tasks (org-asana-sync--request "GET" (format "/tasks/search/by_id?workspace=%s" workspace) nil tasks-query-params)))
        
        (unless org-asana-sync-team-gid
          (message "Fetching tasks from Asana for user %s in workspace %s..." user-gid workspace)
          (setq team-members (list user-info)) ; Just the current user for tagging purposes
          (setq tasks (org-asana-sync--request
                       "GET"
                       (format "/tasks?assignee=%s&workspace=%s&opt_fields=name,notes,completed,due_on,permalink_url,assignee.gid,assignee.name"
                               user-gid workspace))))
        
        (dolist (task tasks)
          (let* ((gid (map-elt task "gid"))
                 (pos (org-asana-sync--find-entry-by-id gid)))
            (if pos
                (save-excursion
                  (goto-char pos)
                  (org-asana-sync--update-entry-at-point task team-members)
                  (message "Updated: %s" (map-elt task "name")))
              (org-asana-sync--create-entry task team-members)
              (message "Created: %s" (map-elt task "name")))))
        (save-buffer)
        (message "Asana Sync Complete."))))

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
         (completed-val (if completed t :json-false)))
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
