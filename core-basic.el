;; Desativa a criação de arquivos de backup (ex: arquivo.el~)
(setq make-backup-files nil)

;; Desativa o auto-save (ex: #arquivo.el#)
(setq auto-save-default nil)

;; Opcional: Desativa a criação de arquivos de lock (.#arquivo.el)
(setq create-lockfiles nil)

;; Melhor a interação do mouse com o Emacs
(xterm-mouse-mode 1)

;; Mover entre as janelas usando Ctrl-(seta direcional)
(windmove-default-keybindings 'control) 

;; Definição da mensagem no rascunhos
(setq initial-scratch-message ";; ==========================================================================\n;; Este buffer é para rascunho de texto e códigos!\n;; Você pode abrir arquivos usando C-x C-f e para abrir diretórios use C-x d\n;; ==========================================================================\n")
