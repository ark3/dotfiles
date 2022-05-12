;;; early-init.el --- -*- lexical-binding: t -*-

;; Documentation: (info "(emacs) Early Init File")
;;                (info "(emacs) Package Installation")

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Prevent Emacs from doing it early.
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; Switch off garbage collection during startup.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1)))

;; Set these settings before the GUI frame is created.

(setq inhibit-splash-screen t)

(defun my/set-up-gui-stuff ()
  "Set up all the GUI-related stuff that may want to be deferred
until there's an actual GUI to be set up."
  ;; set-face-attribute leaks implementation junk. In short, it calls a
  ;; per-attribute helper, and that helper insists on getting a valid font after
  ;; each call, so if font-a doesn't have a particular weight or whatever,
  ;; setting the weight will override the font face, etc. Running through these
  ;; twice should take care of that.
  ;;
  ;; See also: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45920
  ;; (setq my/fixed-font )
  ;; (setq my/fixed-font "JetBrainsMono Nerd Font")
  (setq my/fixed-font
        ;; "Cascadia Code"
        ;; "Menlo"
        ;; "Iosevka Term SS09"
        "IBM Plex Mono"
        ;; "JetBrainsMono Nerd Font"
        ;; "Source Code Pro"
        ;; "Fira Code"
        )
  (dotimes (_ 2)
    (set-face-attribute 'default nil
                        ;; :font my/fixed-font :width 'expanded  ;;; for Iosevka
                        :font my/fixed-font
                        :weight 'medium
                        :height 140)
    ;; (copy-face `default `fixed-pitch) ; this copies unwanted things, like foreground color
    (set-face-attribute 'fixed-pitch nil
                        ;; :font my/fixed-font :width 'expanded
                        :font my/fixed-font
                        :weight 'medium
                        :height 1.0)
    (set-face-attribute 'variable-pitch nil
                        ;; :font "Source Sans Pro"
                        ;; :font "FiraGo"
                        ;; :font "ia Writer Duospace"
                        ;; :font "PT Sans Caption"
                        :font "IBM Plex Sans"
                        ;; :font "IBM Plex Serif"
                        ;; :font "Iosevka Aile" :weight 'medium
                        :weight 'medium
                        :height 1.15)
    )
  (setq default-frame-alist
        (list
         `(font . ,my/fixed-font)
         '(menu-bar-lines . 1)
         '(tool-bar-lines . 0)
         '(vertical-scroll-bars . nil)
         ;; pixels
         '(internal-border-width . 1) ; pixels
         '(line-spacing . 1)
         ;; characters
         '(width . 120)
         '(height . 50)))
  (setq initial-frame-alist default-frame-alist)
  )

(my/set-up-gui-stuff)
(add-hook 'after-init-hook 'my/set-up-gui-stuff)
(add-hook 'server-after-make-frame-hook 'my/set-up-gui-stuff)
