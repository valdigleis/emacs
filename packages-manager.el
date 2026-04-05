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
  :defer t)

(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons)
  :config (treemacs-load-theme "nerd-icons"))

(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-set-icons t           ; Ativa o suporte a ícones
        centaur-tabs-icon-type 'nerd-icons ; Define especificamente para usar nerd-icons
        centaur-tabs-set-modified-marker t ; Mostra um sinal se o arquivo foi editado
        centaur-tabs-style "bar"           ; Estilo visual das abas
        centaur-tabs-height 32)            ; Altura da aba para acomodar bem os ícones
  (centaur-tabs-mode t))
