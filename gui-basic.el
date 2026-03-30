;; Define a fonte padrão usada no emacs (a fonte tem que esta instalado no sistema)
(set-face-attribute 'default nil 
                    :family "Hack Nerd Font" 
                    :height 120 
                    :weight 'regular)

;; Garante que a fonte fixa (usada em tabelas e código) também seja a Hack
(set-face-attribute 'fixed-pitch nil 
                    :family "Hack Nerd Font" 
                    :height 120)

;; Ativa números de linha relativos globalmente
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)

;; Dica extra: desativar barras de ferramentas inúteis para ganhar espaço
(menu-bar-mode -1)

;; Desativa o tool-bar (Apenas modo gráfico)
(when (display-graphic-p)
  (tool-bar-mode -1))

;; Desativa a barra de rolagem (Apenas modo gráfico)
(when (display-graphic-p)
  (scroll-bar-mode -1))

;; Ativa o destaque para a linha sob o cursor
(global-hl-line-mode t)

;; Ativa o destaque dos parênteses e remove o atraso no destaque
(show-paren-mode t)
(setq show-paren-delay 0)
