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
                            (height . 60)  ; characters
                            (foreground-color . "#fff")
                            (background-color . "#000") ; avoid blast of white
                            (vertical-scroll-bars . nil))
      initial-frame-alist default-frame-alist
      frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      display-line-numbers-type t
      indicate-buffer-boundaries t
      indicate-unused-lines t)

(setq my/font
      (font-spec :family "IBM Plex Mono" :size 11.0 :weight 'medium)
      ;; (font-spec :family "Cascadia Code" :size 13.0 :weight 'regular)
      ;; (font-spec :family "Iosevka Comfy Wide" :size 13.0 :weight 'regular)
      my/variable-pitch-font
      (font-spec :family "IBM Plex Sans" :size 13.0)
      ;; (font-spec :family "IBM Plex Serif" :size 15.0)
      ;; (font-spec :family "PT Serif" :size 15.0)
      ;; (font-spec :family "Charter" :size 16.0)
      ;; (font-spec :family "Cambria" :size 16.0)
      ;; (font-spec :family "Iosevka Comfy Wide Duo" :size 14.0)
      ;; (font-spec :family "ia Writer Duospace" :size 11.0)
      ;; (font-spec :family "ia Writer Duo V" :size 11.0 :weight 'medium)
      )

;; (face-attribute 'default :height nil)
;; (set-face-attribute 'default nil :height 110)
;; (set-face-attribute 'default nil :family "IBM Plex Mono" :weight 'medium)
;; (set-face-attribute 'default nil :family "Cascadia Code" :weight 'regular)
;; (set-face-attribute 'default nil :family "Intel Mono One" :weight 'regular)
;; (set-face-attribute 'variable-pitch nil :height 120)
;; (set-face-attribute 'variable-pitch nil :family "iA Writer Duo V" :weight 'book)
;; (set-face-attribute 'variable-pitch nil :family "IBM Plex Sans" :weight 'regular :height 130)
;; (font-family-list)
;; (set-frame-size nil 220 64)

(when (find-font my/font)
  (add-to-list 'default-frame-alist
               `(font . ,(format "%s %s"
                                 (font-get my/font :family)
                                 (truncate (font-get my/font :size))))))

(add-to-list 'default-frame-alist
             (if (memq window-system '(mac ns))
                 '(menu-bar-lines . 1)
               '(menu-bar-lines . 0)))

(add-hook 'after-init-hook
          (lambda ()
            (when (find-font my/font)
              (set-face-attribute 'default nil :font my/font)
              (set-face-attribute 'fixed-pitch nil :font my/font))
            (when (find-font my/variable-pitch-font)
              (set-face-attribute 'variable-pitch nil :font my/variable-pitch-font))
            (when (< 640 (display-pixel-width) 2000)
              (set-frame-parameter nil 'fullscreen 'maximized))
            (when (< 2000 (display-pixel-width))
              (set-frame-parameter nil 'width 220)))
          -99)
