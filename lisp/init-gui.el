;;; init-gui-frames.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;---------------------------------------------------------------------
;; Cursor
;;---------------------------------------------------------------------
(blink-cursor-mode 0)

;;---------------------------------------------------------------------
;; Window size and features
;;---------------------------------------------------------------------
(setq-default
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(menu-bar-mode -1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;;---------------------------------------------------------------------
;; Windows winner-mode
;;---------------------------------------------------------------------
(add-hook 'after-init-hook 'winner-mode)

;;---------------------------------------------------------------------
;; Line numbers
;;---------------------------------------------------------------------
(global-display-line-numbers-mode 1)
(menu-bar--display-line-numbers-mode-relative)

;;---------------------------------------------------------------------
;; Modeline features (You could learn some codes from it)
;;---------------------------------------------------------------------
(defun wullic/mode-line-format ()
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:eval "%4l:" face mode-line-position-face
		:weight "bold")
   (:eval (propertize "%3c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))

   ; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " RO " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t "      ")))
   "  "
   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b"
                face mode-line-filename-face)
   ; narrow [default -- keep?]
   " %n "
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   "     %["
   ;; (:propertize mode-name
                ;; face mode-line-mode-face)
   (:propertize mode-name)
   "%] "

   ;; (:propertize minor-mode-alist)
   (envrc-mode envrc-lighter)
   ;; (:eval (let ((other-minor-mode (delete 'envrc-mode minor-mode-alist)))
	    ;; (:propertize  envrc-mode 'face 'mode-line-minor-mode-face);;) )
   (:eval (propertize (format-mode-line minor-mode-alist)
                      'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   
   "     "
   (:eval (format-time-string "%m-%d %H:%M"))
   ))  )


;; Helper function
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 1 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :background "gray20"
    :box '(:line-width 1 :color "#eab700"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700")
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")


;;---------------------------------------------------------------------
;; Close emacs welcome screen
;;---------------------------------------------------------------------
(setq inhibit-splash-screen t)         ; hide welcome screen


;;---------------------------------------------------------------------
;; Adjust oppacity
;;---------------------------------------------------------------------
(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

;;---------------------------------------------------------------------
;; Full screen
;;---------------------------------------------------------------------
(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Auto toggle fullscreen
  ;; (global-set-key (kbd "C-<return>") 'toggle-frame-fullscreen)
  (add-hook 'after-init-hook 'toggle-frame-fullscreen)
  )

;;---------------------------------------------------------------------
;; Titlebar
;;---------------------------------------------------------------------
(when *is-a-mac*
  (when (maybe-require-package 'ns-auto-titlebar)
    (ns-auto-titlebar-mode)))


(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

;;---------------------------------------------------------------------
;; Change global font size easily
;;---------------------------------------------------------------------
(require-package 'default-text-scale)
(add-hook 'after-init-hook 'default-text-scale-mode)

(provide 'init-gui)
;;; init-gui.el ends here
