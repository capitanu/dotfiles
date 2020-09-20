(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-enable-at-startup nil)
(setq package-check-signature nil)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme))


(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

(if (daemonp) 
	    (add-hook 'after-make-frame-functions 
		      (lambda (frame) 
			(with-selected-frame frame (load-theme 'spacemacs-dark t)))) 
  (load-theme 'spacemacs-dark t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes '(spacemacs-dark))
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(httpd-port 8181)
 '(package-selected-packages
   '(rust-mode helm-all-the-icons helm-icons all-the-icons treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs yaml-mode graphql-mode fiplr helm-fzf helm hlinum dart-mode flutter-l10n-flycheck flutter json-mode vterm nlinum-relative nlinum tuareg xref-js2 js2-refactor js2-mode markdown-mode haskell-mode csharp-mode kotlin-mode magit neotree elixir-mode emmet-mode yasnippets-java-mode love-minor-mode auto-complete company-shell slime-company slime company-jedi company-irony company-c-headers flycheck-clang-analyzer yasnippet-snippets yasnippet popup-kill-ring impatient-mode exwm symon diminish spaceline company dashboard rainbow-delimiters sudo-edit hungry-delete switch-window elpa-clone rainbow-mode avy smex ido-vertical-mode org-bullets beacon spacemacs-theme which-key use-package))
 '(send-mail-function 'mailclient-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 250 :width normal :foundry "DAMA" :family "UbuntuMono Nerd Font Mono")))))

