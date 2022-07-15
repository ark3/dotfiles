;;; early-init.el --- -*- lexical-binding: t -*-

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

;; GUI frames
(setq inhibit-splash-screen t
      default-frame-alist '((menu-bar-lines . 1)
                            (tool-bar-lines . 0)
                            (vertical-scroll-bars . nil)
                            (background-color . "black") ; avoid blast of white
                            (width . 120)  ; characters
                            (height . 50)) ; characters
      initial-frame-alist default-frame-alist)
(when (< (display-pixel-width) 1800)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))
