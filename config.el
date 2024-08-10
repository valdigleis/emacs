(setq inhibit-startup-message t)

(setq-default indent-tabs-mode nil)

(delete-selection-mode t)

(setq make-backup-files nil)

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

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

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

(defun dk4ll/reaload-settings ()
  (interactive)
  (load-file "~/.config/emacs/init.el"))

(defun dk4ll/open-emacs-config ()
  (interactive)
  (find-file "~/.config/emacs/config.org"))

(defun dk4ll/emacs-personal-files ()
  (interactive)
  (let ((default-directory "~/.config/emacs/"))
    (call-interactively 'find-file)))

(defun dk4ll/emacs-right-terminal ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (vterm))

(defun dk4ll/close-terminal ()
  (interactive)
  (when (string= (buffer-name) "*vterm*")
    (kill-buffer))
  (delete-window)
  (other-window -1))

(global-set-key (kbd "C-c t o") 'dk4ll/emacs-right-terminal)
(global-set-key (kbd "C-c t c") 'dk4ll/close-terminal)

;  (use-package keycast
;    :config
;    (add-to-list 'global-mode-string '("" keycast-mode-line-mode ""))
;    (keycast-mode-line-mode))

;; (use-package evil
;;   :init
;;   (setq evil-want-integration t
;;         evil-want-keybinding nil
;;         evil-vsplit-window-right t
;;         evil-split-window-below t
;;         evil-undo-system 'undo-redo)
;;   (evil-mode))

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (add-to-list 'evil-collection-mode-list 'help)
;;   (evil-collection-init))

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

;(use-package neotree
;  :bind
;  (:map global-map
;        ("C-c t" . neotree-toggle)))

(use-package vterm  
  :config
  (setq shell-file-name "/usr/bin/zsh"
        vterm-max-scrollback 5000))

;; (use-package general
;;   :config
;;   (general-evil-setup)
;;   (general-create-definer dk4ll/leader-keys
;;     :states '(normal insert visual emacs)
;;     :keymaps 'override
;;     :prefix "SPC"
;;     :global-prefix "M-SPC")
;;    (dk4ll/leader-keys
;;      "b" '(:ignore t :wk "Buffers/Bookmarks")
;;      "b s" '(switch-to-buffer :wk "Switch to buffer")
;;      "b i" '(ibuffer :wk "Ibuffer")
;;      "b k" '(kill-current-buffer :wk "Kill current buffer")
;;      "b w" '(basic-save-buffer :wk "Save buffer")
;;      "b l" '(list-bookmarks :wk "List bookmarks")
;;      "b m" '(bookmark-set :wk "Set bookmark")
;;      "b q" '(save-buffers-kill-terminal :wk "Quit emacs"))
;;    (dk4ll/leader-keys
;;      "c"   '(:ignore t :wk "Codes commands")     
;;      "c TAB" '(comment-line :wk "Comment lines"))
;;    (dk4ll/leader-keys
;;      "e" '(:ignore t :wk "File explorer")
;;      "e o" '(treemacs :wk "Open file explorer"))
;;    (dk4ll/leader-keys
;;      "f" '(:ignore t :wk "Files")
;;      "f f" '(find-file :wk "Find file"))
;;    (dk4ll/leader-keys
;;      "s" '(:ignore t :wk "Emacs...")
;;      "s c" '(dk4ll/open-emacs-config :wk "Open emacs config.org")
;;      "s p" '(dk4ll/emacs-personal-files :wk "Open personal Emacs config folder")
;;      "s r" '(dk4ll/reaload-settings :wk "Reload Emacs settings"))
;; )

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
