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
(setq default-frame-alist
      (list
       '(menu-bar-lines . 1)
       '(tool-bar-lines . 0)
       '(vertical-scroll-bars . nil)
       ;; pixels
       '(internal-border-width . 1) ; pixels
       ;; characters
       '(width . 120)
       '(height . 50)))
(setq initial-frame-alist default-frame-alist)
