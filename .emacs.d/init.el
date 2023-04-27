(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 2)
 '(package-selected-packages
   '(origami graphql-mode coverlay rjsx-mode prettier-js company-suggest rustic protobuf-mode rust-mode yaml-mode emmet-mode kotlin-mode elixir-mode company-shell slime-company slime company-irony company-c-headers flycheck-clang-analyzer doom-modeline avy company lsp-mode which-key direnv go-mode eglot counsel multi-vterm vterm popup-kill-ring switch-window magit org-bullets rainbow-delimiters doom-themes use-package))
 '(typescript-indent-level 2)
 '(vterm-max-scrollback 100000)
 '(warning-suppress-log-types '((direnv))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
