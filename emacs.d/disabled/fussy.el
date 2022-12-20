(use-package liquidmetal)
(use-package fussy
  ;; https://github.com/jojojames/fussy
  :after (liquidmetal orderless)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'completion-category-overrides
                 '(eglot (styles fussy basic orderless))))
  (setq completion-styles (cons 'fussy completion-styles) ; beginning
        fussy-score-fn 'fussy-liquidmetal-score
        fussy-filter-fn 'fussy-filter-orderless
        completion-category-defaults nil
        completion-category-overrides nil))
