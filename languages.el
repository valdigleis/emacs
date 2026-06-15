;; ---------------------------------------------------------------------------------------
;; Arquivo para configuração dos servidores de linguagens
;; Autor: Valdigleis (valdigleis@gmail.com)
;; Data: 19/05/2026
;; ---------------------------------------------------------------------------------------


;; Configura o eglot para usar os LSP's
(use-package eglot
  :hook ((LaTeX-mode . eglot-ensure)
         (latex-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(LaTeX-mode . ("texlab")))

  (add-to-list 'eglot-server-programs
               '(latex-mode . ("texlab"))))



;; Configuração adicional para o Latex
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)
(setq TeX-save-query nil)
(setq TeX-show-compilation t)

(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
