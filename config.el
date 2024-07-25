(setq inhibit-startup-message t)

; Desabilita o menu bar
(menu-bar-mode -1)
; Desabilita a visualização dos espaços em branco no final das linhas
(setopt show-trailing-whitespace nil)
; Melhora efeito visual do texto sublinhado
(setopt x-underline-at-descent-line nil)
; Habilita o destaque da linha atual do cursor
(global-hl-line-mode t)
; Define a cor usada para o destaque da linha
(set-face-background 'hl-line "#5e4a46")
(set-face-foreground 'highlight nil)
; Habilita os indicadores visuais na margem esquerda da janela de edição
(setopt indicate-buffer-boundaries 'left)

; Habilita a exibição do número das linhas
(global-display-line-numbers-mode 1)
; Customizar o formato ('relative, 'visual ou 't para absoluto) dos números de linhas
(setq display-line-numbers-type 'relative)
; Desabilitar números de linha para modos específicos
(dolist (mode '(org-mode-hook
	      term-mode-hook
	      shell-mode-hook
	      eshell-mode-hook
	      help-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

; Desativar backups globalmente
(setq make-backup-files nil)
; Habilita o salvamento da posição do cursor ao sair
(save-place-mode 1)
; Habilita a funcionalidade de lembrar de arquivos recentes
(recentf-mode 1)
; Habilita o salvamento do historico de comandos
(savehist-mode 1)
; Habilita a atualização automática para os buffers dos arquivos modificados externamente
(setopt auto-revert-avoid-polling t)
; Define o intervalo em segundos que o Emacs irá verificar se houve atualização externa dos arquivos
(setopt auto-revert-interval 60)
; Habilita o Emacs para verificar informações de controle de versão automaticamente
(setopt auto-revert-check-vc-info t)
; Habilita a movimentação entre janelas usando da combinação C-teclas direcionais
(windmove-default-keybindings 'control)
; Desabilita a necessidade das sentenças terem que terminar com dois espaços
(setopt sentence-end-double-space nil)
