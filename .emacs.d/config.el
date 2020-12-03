(defun open-buffer-with (txt)
"create a new buffer, insert txt"
(switch-window)
(find-file txt)
)

(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

(transient-mark-mode 1)

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))
  (global-set-key (kbd "C-c l") 'select-current-line)

(use-package rainbow-delimiters
      :ensure t
      :init )
(add-hook 'org-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(setq backup-inhibited t)
(setq auto-save-default nil)

(global-prettify-symbols-mode 1)

(setq electric-pair-pairs '(
			      (?\( . ?\))
			      (?\[ . ?\])
			      (?\{ . ?\})
			      (?\" . ?\")
			      ))

(defun syntax-for-org ()
(interactive)
(modify-syntax-entry ?< ".")
(modify-syntax-entry ?> "."))
(add-hook 'org-mode-hook 'syntax-for-org)


  (electric-pair-mode 1)

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

(use-package avy
  :ensure t
  :bind
  ("M-s" . avy-goto-char))

(defun copy-whole-line ()
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
     (point-at-bol)
     (point-at-eol)))))
(global-set-key (kbd "C-c w l") 'copy-whole-line)

(defun config-visit ()
  (interactive)
  (find-file "~/repos/github.com/capitanu/dotfiles/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

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

(setq vc-follow-symlinks nil)
(setq org-confirm-babel-evaluate nil)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package vterm
    :ensure t)
  (global-set-key (kbd "<s-M-return>") 'vterm)
(add-hook 'vterm-mode-hook (lambda ()
  (setq-local global-hl-line-mode nil)))

(setq inhibit-startup-message t)
(setq initial-scratch-message ";;  Happy Hacking \n\n")

(setq confirm-kill-processes nil)

(setq x-select-enable-clipboard t)

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

(global-set-key (kbd "C-x M-f") #'er-sudo-edit)

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
(global-set-key (kbd "M-s-h") 'win-resize-minimize-horiz)
(global-set-key (kbd "M-s-l") 'win-resize-enlarge-horiz)
(global-set-key [M-s-up] 'win-resize-enlarge-horiz)
(global-set-key [M-s-down] 'win-resize-minimize-horiz)
(global-set-key (kbd "M-s-h") 'win-resize-enlarge-vert)
(global-set-key (kbd "M-s-l") 'win-resize-minimize-vert)

(setq next-line-add-newlines t)

(use-package hl-line
  :ensure t
  :init)
(set-face-background 'hl-line "#131313")
(global-hl-line-mode 1)

(set-face-attribute 'default nil :height 250)

(defun scroll-up-and-next ()
(interactive)
(scroll-up-line 5)
(next-line 5))

(defun scroll-down-and-prev ()
(interactive)
(scroll-down-line 5)
(previous-line 5))

(global-set-key (kbd "M-n") 'scroll-up-and-next)
(global-set-key (kbd "M-p") 'scroll-down-and-prev)

(define-key input-decode-map [?\C-m] [C-m])

(defun next-by-five ()
(interactive)
(next-line 5))


(defun prev-by-five ()
(interactive)
(previous-line 5))

(global-set-key (kbd "<C-m>") 'next-by-five)
(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "C-o") 'prev-by-five)

(defun open-flags ()
   (interactive)
(find-file "/home/calin/KTH/TCOMK3/EN2720_Ethical_Hacking/flags.org"))
(global-set-key (kbd "C-c f") 'open-flags)

(defun open-readme ()
  (interactive)
  (find-file "/home/calin/repos/github.com/capitanu/miking-ipm/README.md"))
(global-set-key (kbd "C-c m") 'open-readme)

(defun open-hailey-app ()
  (interactive)
  (find-file "/home/calin/repos/github.com/hailey/hailey/app/README.md"))
(global-set-key (kbd "C-c h") 'open-hailey-app)

(defun open-kth ()
(interactive)
(find-file "/home/calin/KTH/TCOMK3/"))
(global-set-key (kbd "C-c k") 'open-kth)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("iex" . "src elixir"))
(require 'org-tempo)
;; (setq org-src-window-setup 'current-window)
;;  (add-to-list 'org-structure-template-alist
;;	       '("el" . "src\n"))
;;  (add-to-list 'org-structure-template-alist
;;	       '("iex" . "src\n"))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode)))
  (add-hook 'org-mode-hook 'prettify-symbols-mode))

(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)

(global-set-key (kbd "C-x b") 'ibuffer)

(setq ibuffer-expert t)

(defun kill-curr-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-curr-buffer)

(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer buffer '(display-buffer-same-window))))
  (use-package magit
    :ensure t
    :pin melpa)
(global-set-key (kbd "C-c g") 'magit-status)

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

(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map
   ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-initial-inputs-alist nil)

;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))
(setq doom-modeline-icon 1)
(setq doom-modeline-buffer-file-name-style 'auto)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-buffer-modification-icon t)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-workspace-name t)
(setq doom-modeline-persp-name t)

(setq split-width-threshold 1)
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

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'org-mode-hook 'linum-mode)
(add-hook 'vterm-mode-hook 'linum-mode)

(use-package company
  :ensure t
  :init)
  (add-hook 'after-init-hook 'global-company-mode)

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

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package yasnippet
  :ensure t
  :config
    (use-package yasnippet-snippets
      :ensure t)
    (yas-reload-all))
(yas-global-mode 1)
(add-hook 'yas-minor-mode-hook (lambda ()
  				 (yas-activate-extra-mode 'fundamental-mode)))

(use-package flycheck
  :ensure t)

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
(add-to-list 'load-path "/home/calin/repos/github.com/miking-lang/miking-emacs/")
(require 'mcore-mode)

(use-package emmet-mode
  :ensure t
  :init
  (emmet-mode))
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-expand-yas)
(add-hook 'css-mode-hook 'emmet-expand-yas)

(use-package js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)


(use-package js2-refactor)
(use-package xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(add-to-list 'load-path "/home/calin/.emacs.d/elpa/rust-mode/")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.lalrpop\\'" . rust-mode))
(require 'rust-mode)

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

(use-package haskell-mode
    :ensure t
)
