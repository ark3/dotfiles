;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Abhay Saxena"
      user-mail-address "ark3@email.com")

(setq doom-font (font-spec :family "IBM Plex Mono" :size 14.0 :weight 'medium)
      ;doom-variable-pitch-font (font-spec :family "ia Writer Duospace" :size 14.0)
      doom-variable-pitch-font (font-spec :family "IBM Plex Serif" :size 16.0)
      doom-theme 'modus-vivendi
      display-line-numbers-type t)

(setq org-directory "~/org/"
      tab-width 8
      scroll-margin 2
      modus-themes-hl-line '(accented)
      modus-themes-mixed-fonts t)

(when IS-MAC
  (setq mac-right-option-modifier 'left
        ns-right-option-modifier  'left)
  (map! [s-up] #'beginning-of-buffer
        [s-down] #'end-of-buffer)
  (when (< (display-pixel-width) 1800)
    (add-to-list 'initial-frame-alist '(fullscreen . maximized))))

(map! "M-o" 'switch-window
      "M-`" 'bury-buffer
      "C-z" nil                         ; suspend-frame
      "C-x C-z" nil)                    ; also suspend-frame

(put 'narrow-to-region 'disabled nil)
(auto-save-visited-mode +1)

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-histfile-override nil     ; Don't create .tramp_history
        tramp-default-method "scpx")
  ;; Assume ControlPersist is set in ~/.ssh/config
  (customize-set-variable 'tramp-use-ssh-controlmaster-options nil))

(after! shell
  (setq shell-pushd-regexp (rx (or "pushd" "pd"))
        shell-popd-regexp (rx (or "popd" "od"))
        shell-cd-regexp "cd"
        comint-input-ignoredups t)
  (map! :map shell-mode-map
        "C-l" (lambda () (interactive) (recenter 0))
        "M-p" #'comint-previous-matching-input-from-input
        "M-n" #'comint-next-matching-input-from-input)
  (setq-hook! 'shell-mode-hook scroll-margin 0))

(after! vterm
  (setq vterm-max-scrollback 99000
        vterm-tramp-shells '(("docker" "/bin/bash")
                             ("scpx" "/bin/bash")
                             ("sshx" "/bin/bash"))))

;;; Text

(defun my/text-stuff ()
  "Set things up for text-related modes"
  (setq-local fill-column 100
              visual-fill-column-center-text t
              visual-fill-column-enable-sensible-window-split t)
  (visual-fill-column-mode 1)
  (org-indent-mode 1)
  (display-line-numbers-mode -1)
  (when buffer-read-only
    (outline-show-all)
    (flyspell-mode -1))
  (variable-pitch-mode 1))

(use-package! org-appear
  :defer t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-delay 0.1))

(after! org
  (setq org-hide-emphasis-markers t
        org-startup-folded 'content
        org-export-with-section-numbers nil
        org-export-with-toc nil)
  (add-hook! 'org-mode-hook :append #'my/text-stuff)
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(after! markdown-mode
  ;; https://gist.github.com/jhacksworth/1aaccec3bf645f835f010eceee68bd92
  (defun my/markdown-live-preview-window-xwidget-webkit (file)
   "Preview FILE with xwidget-webkit."
   (let ((uri (format "file://%s" file)))
     (xwidget-webkit-browse-url uri)
     xwidget-webkit-last-session-buffer))
  (set-popup-rule! "^\\*xwidget" :side 'right :size .50 :ttl 0 :quit nil)
  (setq markdown-live-preview-window-function 'my/markdown-live-preview-window-xwidget-webkit)
  (add-hook! 'markdown-mode-hook :append #'my/text-stuff))


;;; Programming

(use-package! eglot-java
  :after eglot
  :config
  (eglot-java-init)
  (defun my/eglot-java-contact (_interactive)
    "Call my substitute for the Java command"
    (seq-let (tag _command &rest args) (eglot-java--eclipse-contact nil)
      (apply #'list tag (substitute-in-file-name "$HOME/.local/bin/java-for-jdt.sh") args)))
  (setcdr (assq 'java-mode eglot-server-programs) #'my/eglot-java-contact))

(after! format
  (setq-hook! 'html-mode-hook +format-with :none) ; Avoid warnings in Markdown live preview
  (set-formatter! 'google-java-format "google-java-format -" :modes 'java-mode))

(add-hook! 'prog-mode-hook
  (display-fill-column-indicator-mode +1)
  (setq show-paren-style 'mixed))       ; show paren if visible, expr otherwise

(setq-hook! '(java-mode-hook c++-mode-hook)
  +format-with-lsp nil             ; use google-java-format/clang-format instead
  fill-column 100)

(after! vc-gutter
  (setq +vc-gutter-in-remote-files t))

;;; Local

(load! "config-doom.el" "~/.local/emacs" t)  ; NOERROR if file does not exist


;;; Original documentation follows

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
