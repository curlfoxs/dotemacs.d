;;; init-org.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))
(setq org-agenda-files (list "~/org/work.org"
                             "~/org/home.org"))
(provide 'init-org)
;;; init-org.el ends here
