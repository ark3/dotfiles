;; https://protesilaos.com/emacs/fontaine
(use-package fontaine
  :straight (fontaine
             :type git :host github :repo "protesilaos/fontaine")
  :config
  (setq fontaine-presets '((t ; our shared fallback properties
                            :default-family nil ; falls back to ... something?
                            :default-weight medium
                            :default-height 140
                            :fixed-pitch-family nil ; falls back to :default-family
                            :fixed-pitch-weight nil ; falls back to :default-weight
                            :fixed-pitch-height 1.0
                            :variable-pitch-family nil
                            :variable-pitch-weight medium
                            :variable-pitch-height 1.2
                            :bold-family nil ; use whatever the underlying face has
                            :bold-weight bold
                            :italic-family nil
                            :italic-slant italic
                            :line-spacing 1)
                           (plex
                            :default-family "IBM Plex Mono"
                            :variable-pitch-family "IBM Plex Sans")
                           (fira
                            :default-family "Fira Code"
                            :line-spacing 2
                            :variable-pitch-family "FiraGo")
                           (source
                            :default-family "Source Code Pro"
                            :variable-pitch-family "Source Sans Pro")
                           (iosevka ; but how to set :width 'expanded ???
                            :default-family "Iosevka Term SS09"
                            :variable-pitch-family "Iosevka Aile")
                           (comfy
                            :default-family "Iosevka Comfy"
                            :default-height 150
                            :variable-pitch-family "Iosevka Comfy Duo"
                            :line-spacing 2)
                           (comfy-wide
                            :default-family "Iosevka Comfy Wide"
                            :variable-pitch-family "Iosevka Aile"
                            :line-spacing 2)
                           (current
                            :default-family "IBM Plex Mono"
                            :variable-pitch-height 1.0
                            :variable-pitch-family "iA Writer Duospace")))
  ;; Also consider
  ;; "Cascadia Code"
  ;; "Menlo"
  ;; "JetBrainsMono Nerd Font"
  ;; "ia Writer Duospace" for variable-pitch (based heavily on IBM Plex Mono)
  (fontaine-set-preset 'current))
