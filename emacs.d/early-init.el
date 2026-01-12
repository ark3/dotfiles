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
      default-frame-alist '((tool-bar-lines . 0)
                            (width . 120)  ; characters
                            (height . 50)  ; characters
                            (foreground-color . "#fff")
                            (background-color . "#000") ; avoid blast of white
                            (vertical-scroll-bars . nil))
      initial-frame-alist default-frame-alist
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      display-line-numbers-type t
      indicate-buffer-boundaries t
      indicate-unused-lines t)

(set-face-attribute 'default nil :family "IBM Plex Mono" :weight 'medium :height 110)
(set-face-attribute 'variable-pitch nil :family "IBM Plex Sans" :weight 'regular :height 100)
;; (set-face-attribute 'variable-pitch nil :family "iA Writer Duo V" :weight 'book)

;; Keep these around to use manually via `C-x C-e'
;; (set-face-attribute 'default nil :family "Cascadia Code" :weight 'regular)
;; (set-face-attribute 'default nil :height 110)
;; (set-face-attribute 'variable-pitch nil :height 120)
;; (font-family-list)
;; (set-frame-size nil 220 64)

(add-to-list 'default-frame-alist
             (if (memq window-system '(mac ns))
                 '(menu-bar-lines . 1)
               '(menu-bar-lines . 0)))
