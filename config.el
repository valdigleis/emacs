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

(add-to-list 'default-frame-alist '(alpha-background . 100))

(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-startup-folded t)

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-edit-src-content-indentation 0)

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;    (use-package evil
;      :init
;      (setq evil-want-integration t
;            evil-want-keybinding nil
;            evil-vsplit-window-right t
;            evil-split-window-below t
;            evil-undo-system 'undo-redo)
;      (evil-mode))

;    (use-package evil-collecton
;      :after evil
;      :config
;      (add-to-list 'evil-collection-mode-list 'help)
;      (evil-collection-init))

(when (display-graphic-p)
 (use-package keycast
   :init
   (add-to-list 'global-mode-string '("" mode-line-keycast))
   (keycast-mode-line-mode)))

(when (display-graphic-p)
  (after! keycast
          (define-minor-mode keycast-mode
          "Show current keys in mode line!"
          :global t
          (if keycast-mode
              (add-hook 'pre-command-hook 'keycast--update t)
            (remove-hook 'pre-command-hook 'keycast--update))))
  (add-to-list 'global-mode-string '("" keycast-mode-line))
  (require 'keycast))

(when (display-graphic-p)
  (use-package vertico
    :bind (:map vertico-map
              ("C-k" . vertico-next)
              ("C-j" . vertico-previous)
              ("C-e" . vertico-exit)
           :map minibuffer-local-map
              ("M-h" . backward-kill-word))
    :custom
    (vertico-cycle t)
    :init
    (vertico-mode)))

(use-package rainbow-mode
:diminish
:hook org-mode prog-mode)
