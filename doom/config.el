;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; - Improve project stuff
;;   - Turn off projectile somehow
;;   - Bind basic project stuff to C-c p
;; - Better leader stuff
;;   - Add to or replace leader and local leader (M-m and M-RET)
;;   - DOOM: Copy-paste evil stuff into default
;; - Maybe add key chords:
;;   https://dangirsh.org/projects/doom-config.html#key-chord-config

;; (setq +workspaces-on-switch-project-behavior nil) will make project changes never create a new workspace.
;; (global-set-key (kbd (format "C-c C-%d" num))
;;                 (intern (format "winum-select-window-%d" num))
;; ;; load use-package configs
;; (dolist-with-progress-reporter
;;     (config
;;      (directory-files "~/.doom.d/use/" t ".*el"))
;;     "Loading local use configs..."
;;   (load-file config))

;; These work well together; maybe try them?
;; :ui indent-guides
;; :editor word-wrap

(setq user-full-name "Abhay Saxena"
      user-mail-address "ark3@email.com")

(setq doom-font (font-spec :family "IBM Plex Mono" :size 14.0 :weight 'medium)
      doom-variable-pitch-font (font-spec :family "ia Writer Duospace" :size 14.0)
      ;; doom-variable-pitch-font (font-spec :family "IBM Plex Serif" :size 16.0)
      doom-theme 'modus-vivendi
      display-line-numbers-type t
      modus-themes-hl-line '(accented)
      modus-themes-completions 'opinionated ;; '((t . (accented))) ;; for newer Modus
      modus-themes-fringes 'subtle
      ;;modus-themes-mixed-fonts t ;; for newer Modus
      ;;modus-themes-variable-pitch-ui t
      indicate-buffer-boundaries t
      indicate-unused-lines t)

;; project changes don't create a new workspace.
(setq +workspaces-on-switch-project-behavior nil)

;; (after! (modus-themes doom-modeline)
;;   (set-face-attribute 'modus-themes-ui-variable-pitch nil :height 0.9))

(setq scroll-margin 2
      tab-width 8)

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'left
        ns-right-option-modifier  'left)
  (map! [s-up] #'beginning-of-buffer
        [s-down] #'end-of-buffer)
  (when (< (display-pixel-width) 1800)
    (add-to-list 'initial-frame-alist '(fullscreen . maximized))))

(map! "M-o" 'other-window
      "M-`" 'bury-buffer
      "<M-up>"    #'scroll-down-command
      "<M-down>"  #'scroll-up-command
      "C-z" nil                         ; suspend-frame
      "C-x C-z" nil)                    ; also suspend-frame

(put 'narrow-to-region 'disabled nil)
(auto-save-visited-mode +1)
(add-function :after after-focus-change-function (cmd! (save-some-buffers t)))

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-histfile-override nil     ; Don't create .tramp_history
        tramp-default-method "scpx")
  ;; temporary override of Doom's optimization
  (setq vc-ignore-dir-regexp
        "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'")
  ;; Assume ControlPersist is set in ~/.ssh/config
  (customize-set-variable 'tramp-use-ssh-controlmaster-options nil))

(after! helpful
  ;; Because I'm not using Doom's popup managements...
  (defun my/helpful-switch-to-buffer (buffer-or-name)
    (if (eq major-mode 'helpful-mode)
        (switch-to-buffer buffer-or-name)
      (pop-to-buffer buffer-or-name)))
  (setq helpful-switch-buffer-function #'my/helpful-switch-to-buffer))

(defun my/term-set-header-message (message)
  (setq-local my/term-header-message (base64-decode-string (or message ""))))

(defun my/term-setup ()
  (my/term-set-header-message (base64-encode-string ""))
  (setq header-line-format
        '(" "
          (:eval (ansi-color-apply my/term-header-message))
          (:eval (string-trim (abbreviate-file-name default-directory) "" "/")))))

(after! shell
  (add-hook! 'comint-output-filter-functions 'comint-osc-process-output)
  (add-hook! 'shell-mode-hook #'my/term-setup)
  (setq comint-terminfo-terminal "ansi"
        comint-scroll-show-maximum-output nil
        comint-input-ignoredups t)
  ;; (map! :map shell-mode-map
  ;;       "M-p" #'comint-previous-matching-input-from-input
  ;;       "M-n" #'comint-next-matching-input-from-input)
  (setq-hook! 'shell-mode-hook
    scroll-margin 0
    recenter-positions '(top bottom middle)))

;; (use-package! bash-completion
;;   :disabled
;;   :config
;;   (bash-completion-setup))

;; (use-package! coterm
;;   :disabled
;;   :config
;;   (coterm-mode))

(after! vterm
  (map! :map vterm-copy-mode-map
        "C-c C-c" #'vterm-copy-mode)
  (add-hook! 'vterm-mode-hook #'my/term-setup)
  (add-to-list 'vterm-eval-cmds '("set" my/term-set-header-message))
  (setq vterm-max-scrollback 99000
        vterm-tramp-shells '(("docker" "/bin/bash")
                             ("scpx" "/bin/bash")
                             ("sshx" "/bin/bash"))))

(use-package! iedit
  :init
  ;; Allow iedit to bind C-;
  (map! "C-;" . nil))

;; Stop hiding the mode line in useful places
;; TODO: Maybe just turn off this package entirely?
(remove-hook!
  ('shell-mode-hook 'vterm-mode-hook 'completion-list-mode-hook)
  #'hide-mode-line-mode)


;;; Text

(use-package! visual-fill-column :defer t)

(defun my/text-stuff ()
  "Set things up for text-related modes"
  (setq-local fill-column 100
              visual-fill-column-center-text t
              visual-fill-column-enable-sensible-window-split t)
  (visual-line-mode 1)
  (visual-fill-column-mode 1)
  (org-indent-mode 1)
  (display-line-numbers-mode -1)
  (when buffer-read-only
    (outline-show-all)
    (flyspell-mode -1))
  (variable-pitch-mode 1))

(use-package! org-appear
  :defer t
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-delay 0.1))

(after! org
  (setq org-export-with-section-numbers nil
        org-export-with-toc nil)
  (add-hook! 'org-mode-hook :append #'my/text-stuff)
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(add-hook! 'doom-docs-mode-hook
  (outline-show-all)
  (visual-fill-column-mode -1)
  (flyspell-mode -1))

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

(after! flymake
  (map! :leader
        (:prefix-map ("!" . "checkers")
         "n" #'flymake-goto-next-error
         "p" #'flymake-goto-prev-error
         "l" #'consult-flymake
         "b" #'flymake-show-buffer-diagnostics
         "B" #'flymake-show-project-diagnostics)))

(after! format-all
  (setq-hook! 'html-mode-hook +format-with :none) ; Avoid warnings in Markdown live preview
  (set-formatter! 'google-java-format "google-java-format -" :modes 'java-mode))

(add-hook! 'prog-mode-hook
  (display-fill-column-indicator-mode +1)
  (format-all-mode)
  (setq tab-width 8
        tab-always-indent 'complete     ; I don't ever want a "real" TAB char
        show-paren-style 'mixed))       ; show paren if visible, expr otherwise

(setq-hook! '(java-mode-hook c++-mode-hook)
  +format-with-lsp nil             ; use google-java-format/clang-format instead
  tab-width 8
  fill-column 100)

(after! eglot
  (set-face-attribute 'eglot-highlight-symbol-face nil :inherit 'match))

(after! diff-hl
  (setq diff-hl-disable-on-remote nil))

(after! magit  ;; should this be git-commit?
  ;; Change these back to their defaults. Doom's settings are dumb.
  (setq git-commit-summary-max-length 68
        git-commit-style-convention-checks '(non-empty-second-line)))

(after! compile
  (setq comint-buffer-maximum-size 99000))

(after! lsp-mode
  (setq lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-segments '(project file symbols)))

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
