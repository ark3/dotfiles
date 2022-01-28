;;; early-init.el --- -*- lexical-binding: t -*-

;; Documentation: (info "(emacs) Early Init File")
;;                (info "(emacs) Package Installation")

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Prevent Emacs from doing it early.
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)

;; Switch off garbage collection (will be switched on later).
;; This is step 1 of 2. Step 2 is in init.
;; Taken from Doom Emacs.
(setq gc-cons-threshold most-positive-fixnum)

;; Set these settings before the GUI frame is created.

(custom-set-variables
 '(tool-bar-mode nil)
 '(scroll-bar-mode nil)
 '(menu-bar-mode t))

(setq default-frame-alist
      (append (list
               ;; pixels
               '(internal-border-width . 1) ; pixels
               ;; characters
               '(width . 120) '(height . 50))))

(setq-default line-spacing 1)

(setq inhibit-splash-screen t
      my/fixed-font "Iosevka Term SS09"
      ;;my/fixed-font "JetBrainsMono Nerd Font"
      ;;my/fixed-font "Source Code Pro"
      )

;; set-face-attribute leaks implementation junk. In short, it calls a
;; per-attribute helper, and that helper insists on getting a valid font after
;; each call, so if font-a doesn't have a particular weight or whatever, setting
;; the weight will override the font face, etc. Running through these twice
;; should take care of that.
;;
;; See also https://debbugs.gnu.org/cgi/bugreport.cgi?bug=45920
(dotimes (_ 2)
  (set-face-attribute 'default nil
		      :font my/fixed-font :height 120
		      :width 'expanded :weight 'medium)
  (set-face-attribute 'fixed-pitch nil
		      :font my/fixed-font :height 120
		      :width 'expanded :weight 'medium)
  ;;(set-face-attribute 'variable-pitch nil :font "ia Writer Duospace" :height 130)
  ;;(set-face-attribute 'variable-pitch nil :font "PT Sans Caption" :height 130)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 130 :weight 'medium)
)
