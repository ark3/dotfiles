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
                            (width . 120)  ; characters
                            (height . 50)  ; characters
                            (foreground-color . "#fff")
                            (background-color . "#000") ; avoid blast of white
                            (vertical-scroll-bars . nil))
      initial-frame-alist default-frame-alist
      display-line-numbers-type t
      indicate-buffer-boundaries t
      indicate-unused-lines t)

(setq my/font (font-spec :family "IBM Plex Mono" :size 14.0 :weight 'medium)
      ;; my/font (font-spec :family "Iosevka Comfy Wide" :size 13.0 :weight 'regular)
      ;; my/variable-pitch-font (font-spec :family "IBM Plex Serif" :size 15.0)
      ;; my/variable-pitch-font (font-spec :family "PT Serif" :size 15.0)
      ;; my/variable-pitch-font (font-spec :family "Charter" :size 16.0)
      ;; my/variable-pitch-font (font-spec :family "Cambria" :size 16.0)
      ;; my/variable-pitch-font (font-spec :family "Iosevka Comfy Wide Duo" :size 14.0)
      my/variable-pitch-font (font-spec :family "ia Writer Duospace" :size 14.0)
      )

(when (find-font my/font)
  (add-to-list 'default-frame-alist
               `(font . ,(format "%s %s"
                                 (font-get my/font :family)
                                 (truncate (font-get my/font :size))))))

(add-hook 'after-init-hook
          (lambda ()
            (when (find-font my/font)
              (set-face-attribute 'default nil :font my/font)
              (set-face-attribute 'fixed-pitch nil :font my/font))
            (when (find-font my/variable-pitch-font)
              (set-face-attribute 'variable-pitch nil :font my/variable-pitch-font))
            (when (< 640 (display-pixel-width) 1800)
              (set-frame-parameter nil 'fullscreen 'maximized)))
          -99)
