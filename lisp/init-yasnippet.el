(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (use-package yasnippet-snippets
    :ensure t)
  ;; (yas-reload-all)
)

(with-eval-after-load 'yasnippet
  (diminish 'yas-global-mode)
  (diminish 'yas-minor-mode))

(provide 'init-yasnippet)
