;;; init-org-roam.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org-roam
  :ensure t
  ;; :demand t  ;; Ensure org-roam is loaded by default
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (expand-file-name "roam" wullic-org-directory))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n L" . org-roam-buffer-display-dedicated)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n I" . org-roam-node-insert-immediate)
	 ;; ("C-c n p" . my/org-roam-find-project)
	 ;; ("C-c n t" . my/org-roam-capture-task)
	 ;; ("C-c n b" . my/org-roam-capture-inbox)
	 ("C-c n a" . org-roam-alias-add)
	 ("C-c n c" . org-id-get-create)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point))
  :config
  (org-roam-db-autosync-mode)
  (add-to-list 'display-buffer-alist
	       '(("\\*org-roam\\*"
		  (display-buffer-in-direction)
		  (direction . right)
		  (window-width . 0.33)
		  (window-height . fit-window-to-buffer))))
  (setq org-roam-capture-templates
      '(("d" "default" plain
	 "%?"
	 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n\n- tags ::      :noexport:\n")
	 :unnarrowed t)

	("l" "programming language" plain
	 "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
	 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n\n- tags ::      :noexport:\n")
	 :unnarrowed t)

	("b" "book notes" plain
	 "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
	 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n\n- tags ::      :noexport:\n")
	 :unnarrowed t)

	("p" "project" plain
	 "- tags ::      :noexport:\n\n* Goals\n\n%?\n\n* Resource\n\n** 巨人的肩膀\n\n** 知识组件\n\n** 鲤鱼跃龙门\n\n* Tasks\n\n** PROJECT Tasks \n\n** TODO Add initial tasks\n\n* Dates\n\n* Notes\n\n"
	 :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n#+filetags: Project\n\n")
	 :unnarrowed t)))
)

;; ;;---------------------------------------------------------------------
;; ;; Insert note more smootherly
;; ;;---------------------------------------------------------------------

;; ;; Bind this to C-c n I
;; (defun org-roam-node-insert-immediate (arg &rest args)
;;   (interactive "P")
;;   (let ((args (cons arg args))
;;	(org-roam-capture-templates (list (append (car org-roam-capture-templates)
;;						  '(:immediate-finish t)))))
;;     (apply #'org-roam-node-insert args)))


;; ;;---------------------------------------------------------------------
;; ;; Build agenda from org roam notes
;; ;;---------------------------------------------------------------------

;; ;; The buffer you put this code in must have lexical-binding set to t!
;; ;; See the final configuration at the end for more details.

;; (defun my/org-roam-filter-by-tag (tag-name)
;;   (lambda (node)
;;     (member tag-name (org-roam-node-tags node))))

;; (defun my/org-roam-list-notes-by-tag (tag-name)
;;   (mapcar #'org-roam-node-file
;;	  (seq-filter
;;	   (my/org-roam-filter-by-tag tag-name)
;;	   (org-roam-node-list))))

;; (defun my/org-roam-refresh-agenda-list ()
;;   (interactive)
;;   (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; ;; Build the agenda list the first time for the session
;; (my/org-roam-refresh-agenda-list)


;; ;;---------------------------------------------------------------------
;; ;; #+category and #+filetags
;; ;;---------------------------------------------------------------------
;; (setq org-roam-capture-templates '(()))

;; ;;---------------------------------------------------------------------
;; ;; Search a list with a tag
;; ;;---------------------------------------------------------------------
;; (defun my/org-roam-project-finalize-hook ()
;;   "Adds the captured project file to `org-agenda-files' if the
;; capture was not aborted."
;;   ;; Remove the hook since it was added temporarily
;;   (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

;;   ;; Add project file to the agenda list if the capture was confirmed
;;   (unless org-note-abort
;;     (with-current-buffer (org-capture-get :buffer)
;;       (add-to-list 'org-agenda-files (buffer-file-name)))))

;; (defun my/org-roam-find-project ()
;;   (interactive)
;;   ;; Add the project file to the agenda after capture is finished
;;   (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

;;   ;; Select a project file to open, creating it if necessary
;;   (org-roam-node-find
;;    nil
;;    nil
;;    (my/org-roam-filter-by-tag "Project")
;;    :templates
;;    '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
;;       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
;;       :unnarrowed t))))

;; (global-set-key (kbd "C-c n p") #'my/org-roam-find-project)


;; ;;---------------------------------------------------------------------
;; ;; Inbox of notes and tasks
;; ;;---------------------------------------------------------------------

;; (defun my/org-roam-capture-inbox ()
;;   (interactive)
;;   (org-roam-capture- :node (org-roam-node-create)
;;		     :templates '(("i" "inbox" plain "* %?"
;;				  :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

;; (global-set-key (kbd "C-c n b") #'my/org-roam-capture-inbox)

;; ;;---------------------------------------------------------------------
;; ;; Capture a task into a specific project
;; ;;---------------------------------------------------------------------
;; (defun my/org-roam-capture-task ()
;;   (interactive)
;;   ;; Add the project file to the agenda after capture is finished
;;   (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

;;   ;; Capture the new task, creating the project file if necessary
;;   (org-roam-capture- :node (org-roam-node-read
;;			    nil
;;			    (my/org-roam-filter-by-tag "Project"))
;;		     :templates '(("p" "project" plain "* TODO %?"
;;				   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
;;							  "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
;;							  ("Tasks"))))))

;; (global-set-key (kbd "C-c n t") #'my/org-roam-capture-task)

;; ;;---------------------------------------------------------------------
;; ;; Auto copy completed tasks to dailies
;; ;;---------------------------------------------------------------------
;; (defun my/org-roam-copy-todo-to-today ()
;;   (interactive)
;;   (let ((org-refile-keep t) ;; Set this to nil to delete the original!
;;	(org-roam-dailies-capture-templates
;;	  '(("t" "tasks" entry "%?"
;;	     :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
;;	(org-after-refile-insert-hook #'save-buffer)
;;	today-file
;;	pos)
;;     (save-window-excursion
;;       (org-roam-dailies--capture (current-time) t)
;;       (setq today-file (buffer-file-name))
;;       (setq pos (point)))

;;     ;; Only refile if the target file is different than the current file
;;     (unless (equal (file-truename today-file)
;;		   (file-truename (buffer-file-name)))
;;       (org-refile nil nil (list "Tasks" today-file nil pos)))))

;; (add-to-list 'org-after-todo-state-change-hook
;;	     (lambda ()
;;	       (when (equal org-state "DONE")
;;		 (my/org-roam-copy-todo-to-today))))

(provide 'init-org-roam)
;; ;;; init-org-roam.el ends here
