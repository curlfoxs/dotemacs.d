(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
              ;; '("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
              ;; '("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
(package-initialize)

;; My packages
(setq prelude-packages (append '(
                                 modus-themes
                                 ;; solarized-theme
                                 ;; ample-theme
                                 ;; color-theme-sanityinc-tomorrow
                                 vscode-dark-plus-theme
                                 drag-stuff
                                 lsp-mode
                                 lsp-treemacs
                                 lsp-python-ms
                                 vue-mode
                                 highlight-symbol
                                 highlight-indent-guides
                                 highlight-parentheses
                                 htmlize
                                 ;; format-all
                                 direnv
                                 workgroups2
                                 meow
                                 wgrep
                                 ripgrep
                                 rg
                                 smart-mode-line
                                 ) prelude-packages))
;; Install my packages
(prelude-install-packages)
