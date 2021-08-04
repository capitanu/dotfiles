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
 '(ivy-mode t)
 '(package-selected-packages
   '(ess python-black fiplr yaml-mode haskell-mode tuareg all-the-icons-ivy yasnippet-snippets rainbow-delimiters rainbow-mode hungry-delete counsel ivy which-key company-irony company-c-headers flycheck-clang-analyzer persp-mode markdown-mode rust-mode xref-js2 js2-refactor js2-mode emmet-mode kotlin-mode elixir-mode company-shell slime-company slime popup-kill-ring impatient-mode company switch-window magit avy org-bullets vterm use-package doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "gray8")))))
