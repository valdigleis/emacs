(setq inhibit-startup-message t)

; Desabilita o menu bar
(menu-bar-mode -1)

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
