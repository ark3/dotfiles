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
(let ((width (display-pixel-width)))
  (message "Display pixel width is %s at startup" width)
  (when (< 640 (display-pixel-width) 1800)
    (message "--> Setting max in initial-frame-alist")
    (add-to-list 'initial-frame-alist '(fullscreen . maximized))))

(setq my/font (font-spec :family "IBM Plex Mono" :size 14.0 :weight 'medium)
      my/font-string "IBM Plex Mono 14"
      ;; my/variable-pitch-font (font-spec :family "IBM Plex Serif" :size 16.0)
      my/variable-pitch-font (font-spec :family "ia Writer Duospace" :size 14.0))

(add-to-list 'default-frame-alist `(font . ,my/font-string))
(add-hook 'after-init-hook
          (lambda ()
            (set-face-attribute 'default nil :font my/font)
            (set-face-attribute 'fixed-pitch nil :font my/font)
            (set-face-attribute 'variable-pitch nil :font my/variable-pitch-font)
            (let ((width (display-pixel-width)))
              (message "Display pixel width is %s at after-init" width)
              (when (< 640 (display-pixel-width) 1800)
                (message "--> Maximizing")
                (set-frame-parameter nil 'fullscreen 'maximized))))
          -99)
