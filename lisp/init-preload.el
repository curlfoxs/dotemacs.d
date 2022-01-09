;;; init-preload.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq native-comp-async-report-warnings-errors nil)
;; (setq warning-suppress-log-types '((package reinitialization)))
(setq byte-compile-warnings '(cl-functions)) ;; Ignore cl-lib warnings


(provide 'init-preload)
;;; init-preload.el ends here
