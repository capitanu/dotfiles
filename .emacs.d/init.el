(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
;;(setq package-enable-at-startup nil)
;;(setq package-check-signature nil)

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

 (modify-syntax-entry ?< ".")
 (modify-syntax-entry ?> ".")


 (unless (package-installed-p 'doom-themes)
   (package-refresh-contents)
   (package-install 'doom-themes))

 (use-package doom-themes
   :config
   (setq doom-themes-enable-bold t    
         doom-themes-enable-italic t)
   (setq doom-font (font-spec :family "monospace" :size 13 :weight 'semi-light)
 	doom-variable-pitch-font (font-spec :family "sans" :size 13))
   (load-theme 'doom-one t)
   (doom-themes-visual-bell-config) 
   (doom-themes-org-config))

(if (daemonp) 
	    (add-hook 'after-make-frame-functions 
		      (lambda (frame) 
			(with-selected-frame frame (load-theme 'doom-one t)))) 
  (load-theme 'doom-one t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(company-idle-delay 0)
 '(company-tooltip-idle-delay 0)
 '(custom-safe-themes
   '("b6a18791e49cf5f505824d307a9e86a893cb4821a066525729469b0c5e0e8203" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" default))
 '(global-company-mode t)
 '(ivy-mode t)
 '(js-indent-level 2)
 '(lsp-ui-sideline-diagnostic-max-lines 10)
 '(package-selected-packages
   '(lsp-ui lsp-mode eslint-fix quelpa tree-sitter-langs typescript-mode editorconfig nlinum multi-vterm rustic rjsx-mode projectile cider better-defaults clojure-mode-extra-font-locking clojure-mode ewal-doom-themes spaceline-all-the-icons all-the-icons-ivy-rich treemacs-all-the-icons all-the-icons-dired all-the-icons-completion all-the-icons-gnus fontawesome web-mode dockerfile-mode docker protobuf-mode scala-mode go-mode cuda-mode ess python-black fiplr yaml-mode haskell-mode tuareg all-the-icons-ivy yasnippet-snippets rainbow-delimiters rainbow-mode hungry-delete counsel ivy which-key flycheck-clang-analyzer persp-mode markdown-mode rust-mode xref-js2 js2-refactor js2-mode emmet-mode kotlin-mode elixir-mode slime popup-kill-ring impatient-mode switch-window magit avy org-bullets vterm use-package doom-themes))
 '(truncate-lines t)
 '(typescript-indent-level 4)
 '(vc-follow-symlinks nil)
 '(vc-handled-backends '(Git))
 '(warning-suppress-types '((undo discard-info))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "gray8")))))
