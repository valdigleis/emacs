; Desabilitar tela de boas-vindas e ativas o scratch
  (setq inhibit-startup-message t)
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
  ; Desabilita o sinal sonoro
  (setq ring-bell-function 'ignore)
  ; Desabilita o sinal visual
  (when (display-graphic-p)
    (setq visible-bell t))
  ; desabilita o sinal visual (no modo gráfico)
  (when (display-graphic-p)
(setq visible-bell t))

; Habilita a exibição do número da coluna
(column-number-mode 1)
; Desabilita o menu bar
(menu-bar-mode -1)
; Desabilita a tool bar (no modo gráfico)
(when (display-graphic-p)
  (tool-bar-mode -1))
; Desabilita a barra de navegação (no modo gráfico)
(when (display-graphic-p)
  (scroll-bar-mode -1))
; Desabilita a tooltip (no modo gráfico)
(when (display-graphic-p)
  (tooltip-mode -1))
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
; Define a borda do Emacs em 10px (no modo gráfico)
(when (display-graphic-p)
  (set-fringe-mode 10))

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

(use-package rainbow-mode
:diminish
:hook org-mode prog-mode)

; Carregar tema
(load-theme 'modus-vivendi)
