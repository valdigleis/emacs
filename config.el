(setq inhibit-startup-message t)

(setq-default indent-tabs-mode nil)

(delete-selection-mode t)

(setq make-backup-files nil)
(setq auto-save-default nil)

(save-place-mode 1)

(recentf-mode 1)

(savehist-mode 1)

(setopt auto-revert-avoid-polling t)

(setopt auto-revert-interval 60)

(setopt auto-revert-check-vc-info t)

(windmove-default-keybindings 'control)

(setopt sentence-end-double-space nil)

(setq ring-bell-function 'ignore)

(when (display-graphic-p)
  (setq visible-bell t))
(when (display-graphic-p)
  (setq visible-bell t))

(fset 'yes-or-no-p 'y-or-n-p)

(electric-pair-mode 1)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-unset-key (kbd "C-z"))

(column-number-mode 1)

(menu-bar-mode -1)

(when (display-graphic-p)
  (tool-bar-mode -1))

(when (display-graphic-p)
  (scroll-bar-mode -1))

(when (display-graphic-p)
  (tooltip-mode -1))

(setopt show-trailing-whitespace nil)

(setopt x-underline-at-descent-line nil)

(global-hl-line-mode t)

(set-face-background 'hl-line "#5e4a46")
(set-face-foreground 'highlight nil)

(global-visual-line-mode t)

(setopt indicate-buffer-boundaries 'left)

(when (display-graphic-p)
  (set-fringe-mode 10))

(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                help-mode-hook))
        (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "FiraMono Nerd Font Mono 13")
(set-face-attribute 'variable-pitch nil :font "FiraMono Nerd Font Mono 13")
(set-face-attribute 'fixed-pitch nil :font "FiraMono Nerd Font Mono 13")

(add-to-list 'default-frame-alist '(alpha-background . 90))

(setq initial-scratch-message ";; ---------------------------------------------\n;; Autor:\n;; Data: \n;; ---------------------------------------------\n")

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(setq create-lockfiles nil)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                          ("elpa" . "https://elpa.gnu.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(defun dk4ll/reaload-emacs-settings ()
  (interactive)
  (load-file "~/.config/emacs/init.el"))

(defun dk4ll/open-emacs-config ()
  (interactive)
  (find-file "~/.config/emacs/config.org"))

(defun dk4ll/emacs-personal-folder ()
  (interactive)
  (let ((default-directory "~/.config/emacs/"))
    (call-interactively 'find-file)))

(defun dk4ll/emacs-right-terminal ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (vterm))

(defun dk4ll/emacs-terminal-close ()
  (interactive)
  (when (string= (buffer-name) "*vterm*")
    (kill-buffer))
  (delete-window)
  (other-window -1))

(defun my-lsp-fix-buffer ()
  "Formats buffer and organizes imports using LSP."
  (interactive)
  (lsp-organize-imports)
  (lsp-format-buffer))

(global-set-key (kbd "C-c t o") 'dk4ll/emacs-right-terminal)
(global-set-key (kbd "C-c t c") 'dk4ll/emacs-terminal-close)

(use-package vertico
  :bind (:map vertico-map
              ("C-i" . vertico-previous)
              ("C-o" . vertico-next)
              ("C-e" . vertico-exit)
         :map minibuffer-local-map
              ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)))

(use-package consult)

(use-package which-key
  :init
  (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-allow-imprecise-window-fit nil
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 8
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.9
        which-key-allow-imprecise-widow-fit nil
        which-key-separator " » "))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-oceanic-next t)
  (doom-themes-org-config))

;(use-package nano-theme
;  :defer t
;  :quelpa (nano-theme
;           :fetcher github
;           :repo "rougier/nano-theme"))
;(nano-light)

(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 35)
  (set-face-attribute 'mode-line nil :font "FiraMono Nerd Font Mono" :height 110)
  (set-face-attribute 'mode-line-inactive nil :font "FiraMono Nerd Font Mono" :height 110)
  (setq doom-modeline-enable-word-count t))

;(use-package nano-modeline
;  :custom
;  (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
;  (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
;  (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
;  (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
;  (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
;  (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
;  (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
;  (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
;  (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
;  (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
;  (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
;  (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
;  (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode))
;(nano-modeline-text-mode t)

(use-package diminish)

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)
         (emcas-lisp-mode . rainbow-delimiters-mode)
         (cloujure-mode . raindow-delimiters-mode)))

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package treemacs
  :bind
  (:map global-map
        ("C-c f" . treemacs))
  :config
  (setq treemacs-no-png-images t
        treemacs-is-never-other-window nil
        treemacs-show-hidden-files t
        treemacs-git-mode 'deferred))

(require 'treemacs-nerd-icons)
(treemacs-load-theme "nerd-icons")

(use-package vterm  
  :config
  (setq shell-file-name "/usr/bin/zsh"
        vterm-max-scrollback 5000))

(use-package flycheck
  :hook (prog-mode-hook . flycheck-mode))

(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode))

(use-package magit)

(use-package rainbow-mode
:diminish
:hook org-mode prog-mode)

(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-startup-folded t)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)

(add-hook 'org-mode-hook (lambda ()
                           (setq-local electric-pair-inhibit-predicate
                                       '(lambda (c)
                                          (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)

(require 'org-tempo)

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("" "" "◆" "◇" "▪" "▪" "▪")))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (setq markdown-command "pandoc")
  (setq markdown-asymmetric-header t)
  (setq markdown-header-scaling t)
  (setq markdown-enable-math t))

(use-package markdown-preview-mode
  :commands markdown-preview)

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.css\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-comment-keywords t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-comment-style 2)
  (setq web-mode-style-padding 1)
  (setq web-mode-script-padding 1)
  (setq web-mode-block-padding 0))

(use-package json-mode)

(use-package tuareg
  :custom
  (tuareg-opam-insinuate t)
  :config)
(use-package dune-format)
(use-package reason-mode)

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-jedi)

(use-package lsp-haskell)

(use-package lsp-java)

(use-package lsp-mode
  :after flycheck
  :commands lsp
  :bind (("C-c l n" . flycheck-next-error)
         ("C-c l d" . lsp-find-definition)
         ("C-c l r" . lsp-find-references)
         ("C-c l p" . lsp-describe-thing-at-point)
         ("C-c l i" . lsp-find-implementation)
         ("C-c l R" . lsp-rename)
         ("C-c l f" . my-lsp-fix-buffer))
  :hook ((c-mode . lsp)
	     (c++-mode . lsp)
         (python-mode . lsp)
         (haskell-mode . lsp)
         (web-mode . lsp)
         (java-mode . lsp)
         (tuareg-mode . lsp)
         (caml-mode . lsp)
         (reason-mode . lsp)
         (before-save . lsp-organize-imports))
  :config
  (setq lsp-clients-clangd-executable "/usr/bin/clangd")
  (setq lsp-completion-provider :capf))

(use-package lsp-ui
  :commands lsp-ui-mode
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (lsp-ui-doc-enable t))
