;;; init-git.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:

;; See also init-github.el.

;;; Code:

;; TODO: link commits from vc-log to magit-show-commit
;; TODO: smerge-mode
(require-package 'git-blamed)
(require-package 'git-modes)
(when (maybe-require-package 'git-timemachine)
  (global-set-key (kbd "C-x v t") 'git-timemachine-toggle))


;;---------------------------------------------------------------------
;; magit magic~
;;---------------------------------------------------------------------
(when (maybe-require-package 'magit)
  (setq-default magit-diff-refine-hunk t)

  ;; Magit create new branch throws "not a valid starting-point" error
  ;; @see https://github.com/magit/magit/issues/3647
  (setq magit-branch-read-upstream-first 'fallback)

  
  ;; Hint: customize `magit-repository-directories' so that you can use C-u M-F12 to
  ;; quickly open magit on any one of your projects.
  (global-set-key [(meta f12)] 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch)

  (defun sanityinc/magit-or-vc-log-file (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))

  (with-eval-after-load 'vc
    (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file)))


(with-eval-after-load 'magit
  (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up))

(maybe-require-package 'magit-todos)

(require-package 'fullframe)
(with-eval-after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(when (maybe-require-package 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

(when *is-a-mac*
  (with-eval-after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))


;; Convenient binding for vc-git-grep
(with-eval-after-load 'vc
  (define-key vc-prefix-map (kbd "f") 'vc-git-grep))


;;---------------------------------------------------------------------
;; git-svn support
;;---------------------------------------------------------------------

;; (when (maybe-require-package 'magit-svn)
;;   (require-package 'magit-svn)
;;   (autoload 'magit-svn-enabled "magit-svn")
;;   (defun sanityinc/maybe-enable-magit-svn-mode ()
;;     (when (magit-svn-enabled)
;;       (magit-svn-mode)))
;;   (add-hook 'magit-status-mode-hook #'sanityinc/maybe-enable-magit-svn-mode))

(with-eval-after-load 'compile
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))
(defvar git-svn--available-commands nil "Cached list of git svn subcommands")
(defun git-svn--available-commands ()
  (or git-svn--available-commands
      (setq git-svn--available-commands
            (sanityinc/string-all-matches
             "^  \\([a-z\\-]+\\) +"
             (shell-command-to-string "git svn help") 1))))

(autoload 'vc-git-root "vc-git")

(defun git-svn (dir command)
  "Run a git svn subcommand in DIR."
  (interactive (list (read-directory-name "Directory: ")
                     (completing-read "git-svn command: " (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn " command))))

;;---------------------------------------------------------------------
;; diff-hl Amazing~
;;---------------------------------------------------------------------
(require-package 'diff-hl)
(add-hook 'after-init-hook 'global-diff-hl-mode)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(with-eval-after-load 'diff-hl
  (define-key diff-hl-mode-map
	      (kbd "<left-fringe> <mouse-1>")
	      'diff-hl-diff-goto-hunk))
(maybe-require-package 'browse-at-remote)

;;---------------------------------------------------------------------
;; delta
;;---------------------------------------------------------------------
;; @see delta https://github.com/dandavison/delta
;; Install package delta and add following lines to .gitconfig
;; [core]
;;     pager = delta

;; [interactive]
;;     diffFilter = delta --color-only
;; [add.interactive]
;;     useBuiltin = false # required for git 2.37.0

;; [delta]
;;     navigate = true    # use n and N to move between diff sections
;;     light = false      # set to true if you're in a terminal w/ a light background color (e.g. the default macOS terminal)

;; [merge]
;;     conflictstyle = diff3

;; [diff]
;;     colorMoved = default
(when (and (executable-find "delta") (maybe-require-package 'magit-delta))
  (add-hook 'magit-mode-hook (lambda() (magit-delta-mode +1))))

(provide 'init-git)
;;; init-git.el ends here
