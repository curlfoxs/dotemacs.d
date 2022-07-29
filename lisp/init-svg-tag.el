;;; init-svg-tag.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(maybe-require-package 'svg-tag-mode)

(use-package svg-tag-mode
  :commands svg-tag-mode
  :hook ((text-mode . svg-tag-mode)
	 (org-mode . svg-tag-mode))
  :config

  (setq svg-tag-tags-local1
	'(
	  ;; Replaces any occurence of :XXX: with a dynamic SVG tag displaying XXX
	  ("\\(:[a-zA-Z0-9]+:\\)" . ((lambda (tag)
				    (svg-tag-make tag :beg 1 :end -1))))
	  ;; Replaces any occurence of :XXX:YYY: with two adjacent dynamic SVG tags displaying XXX and YYY
	  ("\\(:[a-zA-Z0-9]+\\):[a-zA-Z0-9]+:" . ((lambda (tag)
					   (svg-tag-make tag :beg 1 :inverse t
							  :margin 0 :crop-right t))))
	  (":[a-zA-Z0-9]+\\(:[a-zA-Z0-9]+:\\)" . ((lambda (tag)
					   (svg-tag-make tag :beg 1 :end -1
							 :margin 0 :crop-left t))))
  ))

  (defun wullic/gen-svg-tags (kf)
    (let ((k (concat "\\(" (car kf) "\\)"))
	  (f (cdr kf)))
	(message (concat "todo keyword:" k))
	`(,k . ((lambda (tag) (svg-tag-make tag :inverse t))))))

  (setq svg-tag-tags-local2 (mapcar 'wullic/gen-svg-tags org-todo-keyword-faces))
  (setq svg-tag-tags (append svg-tag-tags-local1 svg-tag-tags-local2))
  )


(provide 'init-svg-tag)
;;; init-svg-tag.el ends here
