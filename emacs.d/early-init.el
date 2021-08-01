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
               '(width . 80) '(height . 50))))


(setq inhibit-splash-screen t)
