;;; my-eglot.el --- eglot configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my configuration file for eglot, https://github.com/joaotavora/eglot
;;; Code:

;;; C development based on https://github.com/MaskRay/ccls/issues/191#issuecomment-556983460
(require-package 'eglot)

;;---------------------------------------------------------------------
;; ccls config
;;---------------------------------------------------------------------

;; Add execute file findding
(with-eval-after-load 'eglot
  (when (executable-find "ccls")
    ;;---------------------------------------------------------------------
    ;; c++-mode
    ;;---------------------------------------------------------------------
    (add-to-list 'eglot-server-programs '(c++-mode . ("ccls"
						      "--init"
						      "{
\"clang\": {
\"extraArgs\": [
\"--stdlib=libc++\",
\"-isystem/usr/local/include\",
\"-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1\",
\"-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/13.0.0/include\",
\"-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include\",
\"-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include\",
\"-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks\"
],
\"resourceDir\": \"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/13.0.0\"
}
}")))
    ;;---------------------------------------------------------------------
    ;; c-mode
    ;;---------------------------------------------------------------------

    (add-to-list 'eglot-server-programs '(c-mode . ("ccls"
						    "--init"
						    "{
\"clang\": {
\"extraArgs\": [
\"-isystem/usr/local/include\",
\"-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1\",
\"-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/13.0.0/include\",
\"-isystem/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include\",
\"-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include\",
\"-isystem/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks\"
],
\"resourceDir\": \"/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/13.0.0\"
}
}")))))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;;---------------------------------------------------------------------
;; tabwidth
;;---------------------------------------------------------------------
;; (setq-default c-basic-offset 4)
(require-package 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;;---------------------------------------------------------------------
;; Quickrun
;;---------------------------------------------------------------------
(require-package 'quickrun)
(add-hook 'c-mode-common-hook (lambda()
				(quickrun-add-command "c++/c11"
				  '((:command . "g++")
				    (:exec    . ("%c -std=c++11 %o -o %e %s"
						 "%e %a"))
				    (:remove  . ("%e")))
				  :default "c++")))

(provide 'init-c-lsp)

;;; my-eglot.el ends here
