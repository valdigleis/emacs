;; ---------------------------------------------------------------------------------------
;; Arquivo para gerenciamento de pacotes
;; Autor: Valdigleis (valdigleis@gmail.com)
;; Data: 01/04/2026
;; ---------------------------------------------------------------------------------------

;; Bootstrap do straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Integrar com use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Instala/configura o pacote de tema Dracula
(use-package dracula-theme
  :config
  (load-theme 'dracula t))

;; Instala/configura o pacote rainbow-delimeters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Instala/configura o pacote nerd-icons
(use-package nerd-icons)

;; Instala/configura o pacote treemacs
(use-package treemacs
  :defer t
  :bind (:map global-map
	      ("C-c t t" . treemacs)
	      ("C-c t b" . treemacs-bookmark)
	      ("C-c t f" . treemacs-find-file)))

(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :config (treemacs-load-theme "nerd-icons"))

;; Instala/configura o pacote centaur-tabs
(use-package centaur-tabs
  :demand
  :bind
  ("C-c b [" . centaur-tabs-backward)
  ("C-c b ]" . centaur-tabs-forward)
  :config
  (setq centaur-tabs-set-icons t           ;; Ativa o suporte a ícones
        centaur-tabs-icon-type 'nerd-icons ;; Define especificamente para usar nerd-icons
        centaur-tabs-set-modified-marker t ;; Mostra um sinal se o arquivo foi editado
        centaur-tabs-style "bar"           ;; Estilo visual das abas
        centaur-tabs-height 35)            ;; Altura da aba para acomodar bem os ícones
  (centaur-tabs-mode t))

;; Instala/configura o pacote projectile
(use-package projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(with-eval-after-load 'treemacs
  (treemacs-project-follow-mode t))

;; Instala/configura o pacote vertico
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

;; Instala/configura o pacote marginalia
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Instala/configura o pacote nerd-icons-completion
(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  ;; Adiciona ícones também ao Marginalia
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Instala/configura o pacote orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Instala/Configura o pacote AUCTex
(use-package auctex
  :defer t
  :hook (LaTeX-mode . visual-line-mode)
  :hook (LaTeX-mode . flyspell-mode)
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-save-query nil)
  (setq TeX-show-compilation t)
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t))

;; Instala/Configura o pacote Corfu
(use-package corfu
  :straight t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :init
  (global-corfu-mode))

;; Instala/Configura o pacote cape
(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Insla/Configura o pacote cdlatex
(use-package cdlatex
  :straight t
  :hook (LaTeX-mode . turn-on-cdlatex))

;; Instala/Configura o pacote yasnippet
(use-package yasnippet
  :straight t
  :init
  (yas-global-mode 1))

;; Instala/Configura o pacote reftex
(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  :config
  (setq reftex-plug-into-AUCTeX t))


