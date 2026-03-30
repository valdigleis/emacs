(require 'package)

;; Configura os repositórios (MELPA é essencial)
(add-to-list 'package-archives '(("melpa" . "https://melpa.org/packages/")
				 ("gnu"   . "https://elpa.gnu.org/packages/")))

;; Inicializa o sistema de pacotes
(package-initialize)

;; Garante que o use-package baixe o pacote se ele não existir localmente
(require 'use-package)
(setq use-package-always-ensure t)

;; Instala/configura o pacote de tema Dracula
(use-package dracula-theme
  :config
  (load-theme 'dracula t))

;; Instala/configura o pacote rainbow-delimeters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
