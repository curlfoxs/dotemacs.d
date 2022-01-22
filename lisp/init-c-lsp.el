;;; my-eglot.el --- eglot configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This is my configuration file for eglot, https://github.com/joaotavora/eglot
;;; Code:

;;; C development based on https://github.com/MaskRay/ccls/issues/191#issuecomment-556983460
(require-package 'eglot)
(require 'eglot)

;;---------------------------------------------------------------------
;; ccls config
;;---------------------------------------------------------------------

;; Add execute file findding
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
}"))))




(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
;;; Java development Based on https://github.com/joaotavora/eglot/issues/176#issuecomment-445021620
;; (defconst my-eglot-eclipse-jdt-home
;; "/Users/toni/.emacs.d/jdt-language-server-0.52.0-202003111128/plugins/org.eclipse.equinox.launcher_1.5.700.v20200207-2156.jar"
;; "Point to eclipse jdt jar.")

;; (defun my-eglot-eclipse-jdt-contact (interactive)
;; "Contact with the jdt server input INTERACTIVE."
;; (let ((cp (getenv "CLASSPATH")))
;; (setenv "CLASSPATH" (concat cp ":" my-eglot-eclipse-jdt-home))
;; (unwind-protect (eglot--eclipse-jdt-contact nil)
;; (setenv "CLASSPATH" cp))))

;; (setcdr (assq 'java-mode eglot-server-programs) #'my-eglot-eclipse-jdt-contact)

;; (add-hook 'java-mode-hook 'eglot-ensure)

;;---------------------------------------------------------------------
;; tabwidth
;;---------------------------------------------------------------------
;; (setq-default c-basic-offset 4)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(provide 'init-c-lsp)

;;; my-eglot.el ends here
