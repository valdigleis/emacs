;; Define a fonte padrão usada no emacs (a fonte tem que esta instalado no sistema)
(set-face-attribute 'default nil 
                    :font "Hack Nerd Font" 
                    :height 140 
                    :weight 'regular)

;; Garante que a fonte fixa (usada em tabelas e código) também seja a Hack
(set-face-attribute 'fixed-pitch nil 
                    :font "Hack Nerd Font" 
                    :height 280)

;; Ativa números de linha relativos globalmente
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Define as cores para os números das linhas
(set-face-attribute 'line-number nil :foreground "yellow")
(set-face-attribute 'line-number-current-line nil :foreground "white" :weight 'bold)

;; Dica extra: desativar barras de ferramentas inúteis para ganhar espaço
(menu-bar-mode -1)

;; Desativa o tool-bar (Apenas modo gráfico)
(when (display-graphic-p)
  (tool-bar-mode -1))

;; Desativa a barra de rolagem (Apenas modo gráfico)
(when (display-graphic-p)
  (scroll-bar-mode -1))

;; Desativa o tooltips
(tooltip-mode -1)

;; Ativa o destaque para a linha sob o cursor
(global-hl-line-mode t)

;; Ativa o destaque dos parênteses e remove o atraso no destaque
(show-paren-mode t)
(setq show-paren-delay 0)

;; Desativa as boas-Vindas
(setq inhibit-startup-message t)

;; Define a largura do padding left/right da janela de edição
(set-fringe-mode 10)

;; Ativa o aviso de fim buffer (Apenas modo gráfico)
(when (display-graphic-p)
  (setq visible-bell t))

;; Configurando a barra inferior
(setopt tab-bar-show 1)

;; Adiciona a hora e data no tab-bar, quando este estiver visível
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setopt display-time-format "%T %F %a")
(setopt display-time-interval 1)
(display-time-mode)
