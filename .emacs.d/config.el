(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
     (other (next-window))
     (this-buffer (window-buffer this))
     (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)
    )
  )
  (global-set-key (kbd "C-c s") 'swap-buffers-in-windows)

(global-set-key (kbd "s-<f1>")
  (lambda ()
    (interactive)
    (dired "~/")))

(setq auto-window-vscroll nil)
(setq next-line-add-newlines t)

(set-face-attribute 'default nil :height 200)

(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))

(line-number-mode 1)
(column-number-mode 1)

(global-subword-mode 1)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function 'ignore)

(setq electric-pair-pairs '(
			    (?\( . ?\))
			    (?\[ . ?\])
			    (?\{ . ?\})
			    (?\" . ?\")
			    ))
(electric-pair-mode t)

(global-prettify-symbols-mode 1)

;;  (when window-system (global-hl-line-mode t))

(setq scroll-conservatively 100)

(global-set-key (kbd "<s-M-return>") 'vterm)

(setq inhibit-startup-message t)

(use-package vterm
    :ensure t)

(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

(setq confirm-kill-processes nil)

(setq x-select-enable-clipboard t)

(cua-selection-mode t)

(defun er-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
			 (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(global-set-key (kbd "C-x C-r") #'er-sudo-edit)

(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -7))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 7))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -7))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 7))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -7))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 7))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -7))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 7))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -7))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 7))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -7))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 7))))

(global-set-key [M-s-down] 'win-resize-minimize-vert)
(global-set-key [M-s-up] 'win-resize-enlarge-vert)
(global-set-key [M-s-left] 'win-resize-minimize-horiz)
(global-set-key [M-s-right] 'win-resize-enlarge-horiz)
(global-set-key [M-s-up] 'win-resize-enlarge-horiz)
(global-set-key [M-s-down] 'win-resize-minimize-horiz)
(global-set-key [M-s-left] 'win-resize-enlarge-vert)
(global-set-key [M-s-right] 'win-resize-minimize-vert)

;;(setq default-frame-alist
  ;;    '((background-color . "0x282a36")
;;	(foreground-color . "0xbbc5ff")))

(defvar my-term-shell "/bin/bash")
    (defadvice ansi-term (before force-bash)
      (interactive (list my-term-shell)))
    (ad-activate 'ansi-term)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

(use-package beacon
  :ensure t
  :init
  (beacon-mode 1))

(use-package sudo-edit
  :ensure t
  :bind ("s-e" . sudo-edit))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (setq powerline-default-separator (quote arrow))
  (spaceline-spacemacs-theme))

(use-package diminish
  :ensure t
  :init
  (diminish 'hungry-delete-mode)
  (diminish 'beacon-mode)
  (diminish 'company-mode)
  (diminish 'subword-mode)
  (diminish 'which-key-mode)
  (diminish 'rainbow-mode)
  (diminish 'rainbow-delimiters-mode)
  (diminish 'impatient-mode))

(add-to-list 'display-buffer-alist
		      `(,(rx bos "*helm" (* not-newline) "*" eos)
			   (display-buffer-in-side-window)
			   (inhibit-same-window . t)
			   (window-height . 0.3)))
(require 'helm-config)
  (use-package helm
    :ensure t
    :demand
    :bind (("M-x" . helm-M-x)
	   ("C-x C-f" . helm-find-files)
	   ("C-x b" . helm-buffers-list)
	   ("C-x c o" . helm-occur) ;SC
	   ("M-y" . helm-show-kill-ring) ;SC
	   ("C-x r b" . helm-filtered-bookmarks) ;SC
	   ("C-x C-b" . helm-buffers-list))
    :requires helm-config
    :config (helm-mode 1))

;; fuzzy file finder
(use-package fiplr
:ensure t
:config
(setq fiplr-root-markers '(".git" ".svn"))
(setq fiplr-ignored-globs '((directories (".git" ".svn"))
(files ("*.jpg" "*.png" "*.zip" "*~"))))
(global-set-key (kbd "C-z") 'helm-fzf))

(use-package dmenu
  :ensure t
  :bind
  ("s-SPC" . 'dmenu))

(use-package symon
  :ensure t
  :bind
  ("s-h" . 'symon-mode))

(setq org-src-window-setup 'current-window)
(add-to-list 'org-structure-template-alist
	     '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist
	     '("iex" "#+BEGIN_SRC elixir\n?\n#+END_SRC"))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode)))
  (add-hook 'org-mode-hook 'prettify-symbols-mode))

(use-package rainbow-delimiters
    :ensure t
    :init 
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(defun kill-whole-word ()
  (interactive)
  (backward-word)
  (kill-word 1)  )
(global-set-key (kbd "C-c w w") 'kill-whole-word)

(defun copy-whole-line ()
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
     (point-at-bol)
     (point-at-eol)))))
(global-set-key (kbd "C-c w l") 'copy-whole-line)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-M-s-k") 'kill-all-buffers)

(global-set-key (kbd "C-x b") 'ibuffer)

(setq ibuffer-expert t)

(defun kill-curr-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-curr-buffer)

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(use-package neotree
    :ensure t
    :init)
  (global-set-key [f8] 'neotree-toggle)
;;  (global-set-key [f8] 'neotree-hidden-file-toggle)

(use-package magit
  :ensure t
  :pin melpa)

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("q" "w" "e" "r" "a" "s" "d" "f"))
  :bind
  ([remap other-window] . switch-window))

(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)))
  (setq dashboard-banner-logo-title "Hello, darthvader11!"))

(add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'org-mode-hook 'linum-mode)
;;  (add-hook 'company-mode-hook 'linum-mode)

  (require 'hlinum)
  (hlinum-activate)

(use-package rainbow-mode
    :ensure t
    :init
(add-hook 'prog-mode-hook 'rainbow-mode))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;;  (use-package exwm
;;    :ensure t
;;    :config
;;    (require 'exwm-config)
;;    (exwm-config-default))

(use-package impatient-mode
    :ensure t
    :init)
(impatient-mode 1)
(httpd-start)
(defun enable-impatient-mode()
(interactive)
(impatient-mode 1))
(global-set-key (kbd "C-x C-i") 'enable-impatient-mode)

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(use-package yasnippet
  :ensure t
  :config
    (use-package yasnippet-snippets
      :ensure t)
    (yas-reload-all))

(use-package flycheck
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "SPC") #'company-abort))

(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)

(use-package flycheck-clang-analyzer
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-clang-analyzer)
     (flycheck-clang-analyzer-setup)))

(with-eval-after-load 'company
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook 'company-mode))

(use-package company-c-headers
  :ensure t)

(use-package company-irony
  :ensure t
  :config
  (setq company-backends '((company-c-headers
			    company-dabbrev-code
			    company-irony))))

(use-package irony
  :ensure t
  :config
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(add-hook 'python-mode-hook 'yas-minor-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)

  (with-eval-after-load 'company
      (add-hook 'python-mode-hook 'company-mode))

;;  (use-package company-jedi
;;    :ensure t
;;    :config
;;      (require 'company)
;;      (add-to-list 'company-backends 'company-jedi))

  (defun python-mode-company-init ()
    (setq-local company-backends '((company-etags
                                    company-dabbrev-code))))

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

(use-package slime-company
  :ensure t
  :init
    (require 'company)
    (slime-setup '(slime-fancy slime-company)))

(add-hook 'shell-mode-hook 'yas-minor-mode)
(add-hook 'shell-mode-hook 'flycheck-mode)
(add-hook 'shell-mode-hook 'company-mode)

(defun shell-mode-company-init ()
  (setq-local company-backends '((company-shell
                                  company-shell-env
                                  company-etags
                                  company-dabbrev-code))))

(use-package company-shell
  :ensure t
  :config
    (require 'company)
    (add-hook 'shell-mode-hook 'shell-mode-company-init))

(add-hook 'html-mode-hook 'yas-minor-mode)
(add-hook 'html-mode-hook 'company-mode)

(add-hook 'java-mode-hook 'yas-minor-mode)

(with-eval-after-load 'company
  (add-hook 'java-mode-hook 'company-mode))

(add-hook 'css-mode-hook 'yas-minor-mode)
(add-hook 'css-mode-hook 'company-mode)

(add-hook 'css-mode-hook 'yas-minor-mode)
(add-hook 'css-mode-hook 'company-mode)
(unless (package-installed-p 'elixir-mode)
  (package-install 'elixir-mode))

(use-package kotlin-mode
:ensure t)
(add-hook 'kotlin-mode-hook 'yas-minor-mode)
(add-hook 'kotlin-mode-hook 'company-mode)

;; MCore mode
(add-to-list 'load-path "~/KTH/miking-lang/miking-emacs/")
(require 'mcore-mode)

(use-package emmet-mode
  :ensure t
  :init
  (emmet-mode))
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-expand-yas)
(add-hook 'css-mode-hook 'emmet-expand-yas)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)


(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Assuming usage with dart-mode
(use-package dart-mode
  ;; Optional
  :hook (dart-mode . flutter-test-mode))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
	      ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/opt/flutter/"))

;; Optional
(use-package flutter-l10n-flycheck
  :after flutter
  :config
  (flutter-l10n-flycheck-setup))
