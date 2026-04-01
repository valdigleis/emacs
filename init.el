;; Define o diretório de configurações
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Carrega os arquivos de configuração
(load (expand-file-name "core-basic.el" user-emacs-directory))                ;; comportamento básico
(load (expand-file-name "gui-basic.el" user-emacs-directory))                 ;; modificações básicas de interface
(load (expand-file-name "packages-manager.el" user-emacs-directory))          ;; gerenciamento de pacotes
(load (expand-file-name "mykeys.el" user-emacs-directory))                    ;; gerencia atalhos pessoais


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
