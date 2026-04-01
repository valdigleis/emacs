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
