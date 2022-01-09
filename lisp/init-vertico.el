;;; init-vertico.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'vertico)
(require-package 'savehist)
(require-package 'emacs)

;;---------------------------------------------------------------------
;; Basic config
;;---------------------------------------------------------------------
(when (maybe-require-package 'savehist)
  (savehist-mode))

(when (maybe-require-package 'vertico)
  (vertico-mode 1)
  (vertico-mouse-mode)
  (vertico-multiform-mode))

;; find-file feature
(when (maybe-require-package 'use-package)
  (use-package vertico-directory
    :after vertico
    :ensure nil
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(when (maybe-require-package 'emacs)
    ;; TODO Add simple comment
    (defun crm-indicator (args)
      (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
    (setq enable-recursive-minibuffers t))

;;---------------------------------------------------------------------
;; Functions of amazing Wiki codes
;;---------------------------------------------------------------------
;; Automatically shrink Vertico for embark-collect-live
(when (maybe-require-package 'embark)
  (defun +embark-collect-hook ()
  (when (eq embark-collect--kind :live)
    (with-selected-window (active-minibuffer-window)
      (setq-local vertico-resize t vertico-count 0)
      (vertico--exhibit))))
  (add-hook 'embark-collect-mode-hook #'+embark-collect-hook)
  )

;; Adjust number of visible candidates when buffer is resized
;; When resizing the minibuffer (e.g., via the mouse), adjust the number of visible candidates in Vertico automatically.
(defun vertico-resize--minibuffer ()
  (add-hook 'window-size-change-functions
	    (lambda (win)
	      (let ((height (window-height win)))
		(when (/= (1- height) vertico-count)
		  (setq-local vertico-count (1- height))
		  (vertico--exhibit))))
	    t t))



(defun completion-category-sort-function ()
  (alist-get (vertico--metadata-get 'category)
             completion-category-sort-function-overrides))

(defvar completion-category-sort-function-overrides
  '((file . directories-before-files))
  "Completion category-specific sorting function overrides.")

(defun directories-before-files (files)
  ;; Still sort by history position, length and alphabetically
  (setq files (vertico-sort-history-length-alpha files))
  ;; But then move directories first
  (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
         (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))


;; One may still want to allow the completion table sorting function to take precedence. Then this completion-category-sort-function can be used instead:
(defun completion-category-sort-function ()
  (or (vertico--metadata-get 'display-sort-function)
        (alist-get (vertico--metadata-get 'category)
                   completion-category-sort-function-overrides)))


;; Customize highlighting based on completion-category
;; You can customise the highlighting of completion candidates based on completion category. For example those coming from ivy/counsel maybe missing the ability to visually distinguish directories from files in counsel-find-file. You can set up something similar with vertico.
(defun +completion-category-highlight-files (cand)
  (let ((len (length cand)))
    (when (and (> len 0)
               (eq (aref cand (1- len)) ?/))
      (add-face-text-property 0 len 'dired-directory 'append cand)))
  cand)


(defvar +completion-category-hl-func-overrides
  `((file . ,#'+completion-category-highlight-files))
  "Alist mapping category to highlight functions.")


(advice-add #'vertico--arrange-candidates :around
            (defun vertico-format-candidates+ (func)
              (let ((hl-func (or (alist-get (vertico--metadata-get 'category)
                                            +completion-category-hl-func-overrides)
                                 #'identity)))
                (cl-letf* (((symbol-function 'actual-vertico-format-candidate)
                            (symbol-function #'vertico--format-candidate))
                           ((symbol-function #'vertico--format-candidate)
                            (lambda (cand &rest args)
                              (apply #'actual-vertico-format-candidate
                                     (funcall hl-func cand) args))))
                  (funcall func)))))


;; And the following could be added to the previous code to highlight enabled modes in the command palette (M-x).
(defun +completion-category-highlight-commands (cand)
  (let ((len (length cand)))
    (when (and (> len 0)
               (with-current-buffer (nth 1 (buffer-list)) ; get buffer before minibuffer
                 (or (eq major-mode (intern cand)) ; check major mode
                     (ignore-errors (auto-minor-mode-enabled-p (intern cand)))))) ; check minor modes
      (add-face-text-property 0 len '(:foreground "red") 'append cand))) ; choose any color or face you like
  cand)



;; Useful commands from outside minibuffer
;; These are useful if you start doing something in the minibuffer and go to another window before the minibuffer command is finished; eg if you’re using consult-line to move around to different search matches, but also edit the buffer being searched. These commands probably work with other styles of minibuffer in addition to vertico.

(defun down-from-outside ()
  "Move to next candidate in minibuffer, even when minibuffer isn't selected."
  (interactive)
  (with-selected-window (active-minibuffer-window)
    (execute-kbd-macro [down])))

(defun up-from-outside ()
  "Move to previous candidate in minibuffer, even when minibuffer isn't selected."
  (interactive)
  (with-selected-window (active-minibuffer-window)
    (execute-kbd-macro [up])))

(defun to-and-fro-minibuffer ()
  "Go back and forth between minibuffer and other window."
  (interactive)
  (if (window-minibuffer-p (selected-window))
      (select-window (minibuffer-selected-window))
    (select-window (active-minibuffer-window))))


;; Move annotations in front of the candidate
(defun vertico--swap-annotations (result)
  ;; Move annotations only for files
  (if minibuffer-completing-file-name
      (mapcar (lambda (x)
                ;; Swap prefix/suffix annotations
                (list (car x) (concat (string-trim-left (caddr x)) " ") (cadr x)))
              result)
    result))
(advice-add #'vertico--affixate :filter-return #'vertico--swap-annotations)


;; Additions for moving up and down directories in find-file
;; Altered from vertico-directory-up to alternatively remove the entire entry after the last “/”.

(defun vertico-directory-delete-entry ()
  "Delete directory or entire entry before point."
  (interactive)
  (when (and (> (point) (minibuffer-prompt-end))
             (vertico-directory--completing-file-p))
    (save-excursion
      (goto-char (1- (point)))
      (when (search-backward "/" (minibuffer-prompt-end) t)
        (delete-region (1+ (point)) (point-max))
        t))))


;; You can use vertico-directory-enter to enter a directory or open a file depending on what is selected.
;; Update minibuffer history with candidate insertions
;; This advice to vertico-insert can be used to keep track of which directories you have visited in find-file. Normally candidates are only added to the history on vertico-exit (viewing them in a buffer). With this and sorting by history, the most recently visited folders will show up on top.

(defadvice vertico-insert
    (after vertico-insert-add-history activate)
  "Make vertico-insert add to the minibuffer history."
  (unless (eq minibuffer-history-variable t)
    (add-to-history minibuffer-history-variable (minibuffer-contents))))


;; Pre-select previous directory when entering parent directory from within find-file
;; Advise vertico-directory-up to save the directory being exited.
(defvar previous-directory nil
    "The directory that was just left. It is set when leaving a directory and
    set back to nil once it is used in the parent directory.")


(defun set-previous-directory ()
  "Set the directory that was just exited from within find-file."
  (save-excursion
    (goto-char (1- (point)))
    (when (search-backward "/" (minibuffer-prompt-end) t)
      ;; set parent directory
      (setq previous-directory (buffer-substring (1+ (point)) (point-max)))
      ;; set back to nil if not sorting by directories or what was deleted is not a directory
      (when (not (string-suffix-p "/" previous-directory))
        (setq previous-directory nil))
      t)))

;; (advice-add #'vertico-directory-up :before #'set-previous-directory)
;; Advise vertico--update-candidates to select the previous directory.
(define-advice vertico--update-candidates (:after (&rest _) choose-candidate)
    "Pick the previous directory rather than the prompt after updating candidates."
    (cond
     (previous-directory ; select previous directory
      (setq vertico--index (or (seq-position vertico--candidates previous-directory)
                               vertico--index))
      (setq previous-directory nil))))


;;---------------------------------------------------------------------
;; With eval after load
;;---------------------------------------------------------------------
(with-eval-after-load 'vertico
  ;; Prefix current candidate with arrow
  ;; Prefix the current candidate with “» “.
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))

  ;; Customize sorting based on completion category
  ;; The sorting function can be adjusted based on the completion category. See #76 for the discussion.
  ;; try the `completion-category-sort-function' first
  (advice-add #'vertico--sort-function :before-until #'completion-category-sort-function)


  (add-to-list '+completion-category-hl-func-overrides `(command . ,#'+completion-category-highlight-commands))
  )

(provide 'init-vertico)
;;; init-vertico.el ends here
