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
  (setq my/fixed-font "Iosevka Term SS09")
  (dotimes (_ 2)
    (set-face-attribute 'default nil
			;; :font "JetBrainsMono Nerd Font"
			;; :font "Source Code Pro"
			:font my/fixed-font :width 'expanded :weight 'medium
			:height 120)
    (copy-face `default `fixed-pitch)
    (set-face-attribute 'variable-pitch nil
			;; :font "ia Writer Duospace"
			;; :font "PT Sans Caption"
			:font "Iosevka Aile" :weight 'medium
			:height 130)
    )
  (setq default-frame-alist
	(list
	 `(font . ,my/fixed-font)
	 '(menu-bar-lines . 0)
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
