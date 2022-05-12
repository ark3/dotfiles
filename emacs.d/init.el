;;; init.el -*- lexical-binding: t -*-

;;; Commentary:

;; This file has an outline which can be viewed by looking at comments
;; starting with three or more semicolons. `outline-minor-mode' supports this
;; convention by default and helps with navigation. You can also create an
;; occur buffer with the search /^;;;+/.


;;; Preamble

;; It is useful to know the impact of your init file on Emacs startup time so
;; you can avoid introducing slowdowns. There are many ways to do it, but this
;; is very simple and does the trick for me.

(defvar before-user-init-time (current-time)
  "Value of `current-time' when Emacs begins loading `user-init-file'.")

(message "Loading Emacs, pre-init...done (%.3fs)"
         (float-time (time-subtract before-user-init-time
                                    before-init-time)))

(message "Loading %s..." user-init-file)


;;; Package management

;; Set up straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Always use straight to install
(setq straight-use-package-by-default t)

;; Don't defer loading by default
(setq use-package-always-demand t)

;; Prevent Emacs-provided Org from being loaded
(straight-register-package 'org)
(straight-register-package 'org-contrib)


;;; General stuff

(use-package emacs
  :bind (("s-<up>" . beginning-of-buffer)
         ("s-<down>" . end-of-buffer)
         ("M-<down>" . scroll-up-command)
         ("M-<up>" . scroll-down-command)
         ("C-<next>" . View-scroll-line-forward)
         ("C-<prior>" . View-scroll-line-backward)
         ("M-SPC" . cycle-spacing)
         ("M-`" . bury-buffer))
  :init
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        sentence-end-double-space nil
        ring-bell-function 'ignore
        frame-resize-pixelwise t
        frame-inhibit-implied-resize t
        )

  (setq user-full-name "Abhay Saxena"
        user-mail-address "ark3@email.com")

  ;; always allow 'y' instead of 'yes'.
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; default to utf-8 for all the things
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; write over selected text on input... like all modern editors do
  (delete-selection-mode t)

  ;; Save state between sessions
  (setq recentf-exclude `(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"
                          ,(expand-file-name "straight/build/" user-emacs-directory)
                          ,(expand-file-name "eln-cache/" user-emacs-directory)
                          ,(expand-file-name "etc/" user-emacs-directory)
                          ,(expand-file-name "var/" user-emacs-directory))
        recentf-max-saved-items 250

        desktop-restore-frames nil
        desktop-restore-eager t

        history-length 10000
        history-delete-duplicates t
        savehist-save-minibuffer-history t
        )
  (desktop-save-mode 1)
  (savehist-mode 1)                     ; minibuffer history
  (recentf-mode 1)                      ; recently-edited files
  (save-place-mode 1)                   ; cursor location in visited files
  (dolist (symbol '(kill-ring log-edit-comment-ring))
    (add-to-list 'desktop-globals-to-save symbol))

  ;; don't want ESC as a modifier
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Hardly ever want suspend-frame
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))

  ;; But don't mess up window layout. Instead, short-circuit the cond
  ;; expression by defining a do-nothing buffer-quit-function.
  (defadvice keyboard-escape-quit
      (around keyboard-escape-quit-dont-close-windows activate)
    (let ((buffer-quit-function (lambda () ())))
      ad-do-it))

  ;; Don't persist a custom file, this bites me more than it helps
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables :all)     ; fix =defvar= warnings

  ;; stop emacs from littering the file system with backup files
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  ;; autosave files in-place regularly
  ;;; (auto-save-visited-mode t) ;; trying super-save-mode instead

  ;; follow symlinks
  (setq vc-follow-symlinks t
        find-file-visit-truename t)

  ;; Buffer/window stuff
  (winner-mode t)   ;; enable winner mode globally for undo/redo window layout changes
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd (not (and (<= (car this-win-edges)
                                           (car next-win-edges))
                                       (<= (cadr this-win-edges)
                                           (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
                    'split-window-horizontally
                  'split-window-vertically)))
          (delete-other-windows)
          (let ((first-win (selected-window)))
            (funcall splitter)
            (if this-win-2nd (other-window 1))
            (set-window-buffer (selected-window) this-win-buffer)
            (set-window-buffer (next-window) next-win-buffer)
            (select-window first-win)
            (if this-win-2nd (other-window 1))))))
  (defun reposition-buffer ()
    (interactive)
    (let ((this-win-buffer (window-buffer)))
      (winner-undo)
      (set-window-buffer (next-window) this-win-buffer)))
  (global-set-key (kbd "C-x -") #'toggle-window-split)
  (global-set-key (kbd "C-x C--") #'reposition-buffer)
  (global-set-key (kbd "C-x C-=") #'windmove-swap-states-left)

  (customize-set-variable 'mouse-wheel-scroll-amount
                          '(1 ((shift) . hscroll) ((meta) . nil))
                          "Turn off mouse-wheel-text-scale")

  (show-paren-mode t)
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (transient-mark-mode -1)
  (put 'narrow-to-region 'disabled nil)

  ;; Automatically set executable bit (chmod) for files with a shebang (#!)
  ;; In practice, this is really annoying for things with #!/hint/bash etc.
  ;;(add-hook 'after-save-hook
  ;;          'executable-make-buffer-file-executable-if-script-p)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
        native-comp-async-report-warnings-errors nil
        load-prefer-newer t
        )

  ;; Tramp
  (require 'tramp)
  ;;; include the remote PATH in tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-histfile-override nil     ; Don't create .tramp_history
        tramp-default-method "scpx")
  ;;; Assume ControlPersist is set in ~/.ssh/config
  (customize-set-variable 'tramp-use-ssh-controlmaster-options nil)
  ;; Avoid "ls does not support --dired" message on MacOS
  (when (string= system-type "darwin")
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))

  (setq dired-dwim-target t)
  (setq windmove-wrap-around t)
  (setq use-dialog-box nil)
  (setq read-process-output-max (* 1024 1024)) ; 1mb
  (setq window-min-width 40)
  (setq describe-bindings-outline t)

  ;; Scrolling
  (setq scroll-conservatively 10000
        scroll-margin 3
        scroll-preserve-screen-position nil  ; didn't like t, 1 maybe okay
        comint-terminfo-terminal "ansi"
        comint-scroll-show-maximum-output nil)

  ;; Shell
  (setq shell-pushd-regexp (rx (or "pushd" "pd"))
        shell-popd-regexp (rx (or "popd" "od"))
        shell-cd-regexp "cd"
        comint-input-ignoredups t
        )

  ;; Mode line
  (setq mode-line-compact 'long)
  (display-time-mode -1)
  (column-number-mode t)
  )

;; Packages

(use-package diminish)

;; https://github.com/bbatsov/super-save
(use-package super-save
  :diminish super-save-mode
  :config
  (setq super-save-remote-files t
        super-save-auto-save-when-idle t
        super-save-idle-duration 30
        super-save-max-buffer-size 100000
        )
  (super-save-mode +1))

;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :demand
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  )

(use-package hl-line+
  :hook
  (window-scroll-functions . hl-line-flash)
  (focus-in . hl-line-flash)
  (post-command . hl-line-flash)

  :custom
  (global-hl-line-mode nil)
  (hl-line-flash-show-period 0.4)
  (hl-line-inhibit-highlighting-for-modes '(dired-mode vterm-mode))
  (hl-line-overlay-priority -100) ;; sadly, seems not observed by diredfl
  )

(use-package modus-themes
  :init
  (setq modus-themes-hl-line '(intense accented)
        modus-themes-mixed-fonts t)
  :config (load-theme 'modus-vivendi t))

(use-package avy
  :bind
  (("C-c SPC" . avy-goto-char-timer)
   ("C-'" . avy-goto-word-1)))

(use-package iedit)  ; Binds C-;

(use-package ace-window
  :bind
  (("M-o" . ace-window)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1
        which-key-idle-secondary-delay 0.05
        which-key-show-early-on-C-h nil      ; Use embark-prefix-help-command
        )
  )

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("s-." . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
)

;; Use l/r to go back/forward in dired
;; https://github.com/karthink/dired-hist
(use-package dired-hist
  :straight (dired-hist
             :type git :host github :repo "karthink/dired-hist")
  :bind (:map dired-mode-map
              ("l" . dired-hist-go-back)
              ("r" . dired-hist-go-forward)
         )
  :config
  (dired-hist-mode 1)
  )

(use-package consult
  :bind (
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-comman
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-s f" . consult-find)
         ("M-s F" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)           ;; needed by consult-line to detect isearch
         )
  :init
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  (setq consult-project-root-function (lambda ()
                                        (when-let (project (project-current))
                                          (car (project-roots project))))
        consult-preview-key (kbd "M-.")
        consult-narrow-key "<"
        )
  )

(use-package embark-consult
  :after (embark consult))

(use-package wgrep)

(use-package flymake
  :bind (:map flymake-mode-map
         ("C-c f n" . flymake-goto-next-error)
         ("C-c f p" . flymake-goto-prev-error)
         ("C-c f b" . flymake-show-buffer-diagnostics)
         ("C-c f B" . flymake-show-project-diagnostics)
         )
)

(use-package vertico
  :config
  (vertico-mode)
  (setq vertico-resize 'grow-only)

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  )

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (setq dabbrev-case-distinction nil
        dabbrev-case-fold-search t
        dabbrev-case-replace nil)
)

(use-package ibuffer-project
  :bind ("C-x C-b" . ibuffer)
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups
                    (ibuffer-project-generate-filter-groups))))
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-project-use-cache t))


;; Use the `orderless' completion style.
;; Enable `partial-completion' for file path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))))
  )

(use-package fancy-dabbrev
  :diminish fancy-dabbrev-mode
  :bind (("TAB" . fancy-dabbrev-expand-or-indent))
  :config
  (global-fancy-dabbrev-mode)
  (setq fancy-dabbrev-preview-delay 0.3
        fancy-dabbrev-expansion-on-preview-only t
        dabbrev-case-distinction nil    ; different case as different expansions
        dabbrev-case-fold-search t      ; ignore case on search
        dabbrev-case-replace nil)       ; use expansion's case, not abbrev's
  )

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)
)

(use-package edit-server
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t)
  :config (setq edit-server-new-frame nil)
  )

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "JAVA_HOME")
        exec-path-from-shell-arguments '("-l")
        exec-path-from-shell-warn-duration-millis 300)
  :config
  (setq ns-function-modifier 'control)
  (exec-path-from-shell-initialize))

(use-package helpful
  :bind (;; Remap standard commands.
         ([remap display-local-help] . helpful-at-point)
         ([remap describe-function]  . helpful-callable)
         ([remap describe-variable]  . helpful-variable)
         ([remap describe-symbol]    . helpful-symbol)
         ([remap describe-key]       . helpful-key)
         ([remap describe-command]   . helpful-command)
         :map help-map
         ("F" . #'helpful-function)
         ("M-f" . #'helpful-macro)
         ("C" . #'helpful-command)
         )
  )

;;; Text stuff

(defun text-stuff ()
  (setq fill-column 100)
  (visual-fill-column-mode 1)
  (org-indent-mode 1)
  (variable-pitch-mode 1))

(use-package flyspell
  :custom
  (ispell-program-name "aspell")
  )

(use-package visual-fill-column
  :init
  (add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
  (setq visual-fill-column-center-text t
        visual-fill-column-enable-sensible-window-split t
        )
  :config
  )

(use-package org-autolist :diminish)

(use-package org
  :hook ((org-mode . text-stuff)
         (org-mode . org-autolist-mode))
  :custom
  (org-export-backends '(md ascii html beamer odt latex org))
  (org-hide-emphasis-markers t)
  (org-startup-folded 'content)
  (org-export-with-toc nil)
  (org-export-with-section-numbers nil)
  :config
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :hook (markdown-mode . text-stuff)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mkdn\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))


;;; Programming stuff

(add-hook 'prog-mode-hook
          (lambda ()
            (display-line-numbers-mode t)
            (display-fill-column-indicator-mode t)
            (setq fill-column 80
                  show-paren-delay 0
                  show-paren-style 'mixed
                  show-paren-when-point-inside-paren t
                  show-paren-when-point-in-periphery t
                  indent-tabs-mode nil
                  )))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         )
  )

(defun my/vterm-copy-mode-cancel ()
  "Exit vterm-copy-mode without copying anything"
  (interactive)
  (vterm-copy-mode -1))

(defun project-vterm ()
  "Invoke `vterm' in the project's root.
Switch to the project specific term buffer if it already exists."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (project-vterm-name
          (string-replace
           "service-alchemy-" "s-a-"
           (string-replace
            "lib-alchemy-" "l-a-"
            (project-prefixed-buffer-name "vterm")))))
    (unless (buffer-live-p (get-buffer project-vterm-name))
      (unless (require 'vterm nil 'noerror)
        (error "Package 'vterm' is not available"))
      (vterm project-vterm-name)
      (vterm-send-string (concat "cd " default-directory))
      (vterm-send-return))
    (pop-to-buffer-same-window (get-buffer project-vterm-name))))

(use-package vterm
  :bind (("C-c v" . vterm)
         ("C-x p v" . project-vterm)
         :map vterm-mode-map
         ("M-n" . vterm-send-M-n)
         ("M-p" . vterm-send-M-p)
         ("C-y" . vterm-send-C-y)
         :map vterm-copy-mode-map
         ("C-c C-c" . vterm-copy-mode)
         )
  :init
  (setq vterm-always-compile-module t)
  :config
  (setq vterm-max-scrollback 99000
        vterm-tramp-shells '(("docker" "/bin/bash")
                             ("scpx" "/bin/bash")
                             ("sshx" "/bin/bash")))
  (defun my/vterm-set-header-message (host &optional kctx venv git gitc exit)
    (let ((git-color-number (string-to-number (or gitc "0"))))
      (setq-local my/vterm-header-host (or host ""))
      (setq-local my/vterm-header-kctx (or kctx ""))
      (setq-local my/vterm-header-venv (or venv ""))
      (setq-local my/vterm-header-git  (or git ""))
      (setq-local my/vterm-header-gitc (or git-color-number ""))
      (setq-local my/vterm-header-exit (or exit ""))))
  (add-to-list 'vterm-eval-cmds '("set" my/vterm-set-header-message))
  (defun my/vterm-setup ()
    (my/vterm-set-header-message "Starting up...")
    (setq header-line-format
          '(" "
            ;; (:eval
            ;;  (propertize
            ;;   (format-time-string " %H:%M ")
            ;;   'face 'modus-themes-pseudo-header))
            (:eval (propertize ">>" 'face '(:weight bold)))
            " "
            ;; "host:"
            (:eval (propertize my/vterm-header-host 'face
                               (list :foreground (face-foreground 'term-color-yellow)
                                     :weight 'bold)))
            ;; "/kctx:"
            (:eval (propertize my/vterm-header-kctx 'face
                               (list :foreground (face-foreground 'term-color-magenta))))
            ;; "/venv:"
            (:eval (propertize my/vterm-header-venv 'face
                               (list :foreground (face-foreground 'term-color-blue))))
            ;; "/git:"
            (:eval (propertize my/vterm-header-git  'face
                               (list :foreground
                                     (aref ansi-color-names-vector
                                           my/vterm-header-gitc))))
            ;; "/exit:"
            (:eval (propertize my/vterm-header-exit 'face
                               (list :foreground (face-foreground 'ansi-color-red))))
            ;; "// "
            (:eval (string-trim (abbreviate-file-name (or (vterm--get-pwd) "")) "" "/"))
            ))
      )
  :hook (vterm-mode . my/vterm-setup))

(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (defun display-ansi-colors ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  :hook (compilation-filter . my-colorize-compilation-buffer))

(use-package magit
  :init (setq-default git-magit-status-fullscreen t)
  :bind (("C-c g s" . magit-status)
         ("C-c g g" . magit-file-dispatch)
         ("C-x p m" . magit-project-status)
         )
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1  ; fullscreen status
        magit-bury-buffer-function #'magit-restore-window-configuration  ; restore windows on quit
        magit-prefer-remote-upstream t
        project-switch-commands '((project-find-file "Find file")
                                  (project-dired "Dired")
                                  (project-eshell "Eshell")
                                  (project-shell "Shell")
                                  (project-vterm "Vterm")
                                  (magit-project-status "Magit"))
        )
  )

(use-package git-timemachine
  :bind (("C-c g t" . git-timemachine))
  )

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode)  ; do I want this?
  (setq diff-hl-margin-symbols-alist
        '((insert . "┃")
          (delete . "┃")
          (change . "┃")
          (unknown . "?")
          (ignored . "i")))
  )

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package eglot
  :bind (("C-c l a" . eglot-code-actions)
         ("C-c l r" . eglot-rename)
         ("C-c l =" . eglot-format-buffer)
         )
  :hook ((python-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         )
  )

(defun my-eglot-java-contact (_interactive)
  "Call my substitute for the Java command"
  (seq-let (tag _command &rest args) (eglot-java--eclipse-contact nil)
    (apply #'list tag (substitute-in-file-name "$HOME/.local/bin/java-for-jdt.sh") args)))

(use-package eglot-java
  :after eglot
  :config
  (eglot-java-init)
  (setcdr (assq 'java-mode eglot-server-programs) #'my-eglot-java-contact)
  )

(use-package protobuf-mode)
(use-package go-mode)
(use-package graphql-mode)

(use-package tree-sitter-langs
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
)

(use-package tree-sitter
  :diminish
  :config
  (global-tree-sitter-mode)
  :after (tree-sitter-langs)
  )

(use-package cmake-mode)

(use-package yaml-mode)

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg))

(use-package hideshow ; built-in
  ;; https://karthinks.com/software/simple-folding-with-hideshow/
  :diminish
  :bind (:map prog-mode-map
              ("C-<tab>" . hs-cycle)
              ("C-S-<tab>" . hs-global-cycle)
              ("<backtab>" . hs-global-cycle)
              ("C-S-<iso-lefttab>" . hs-global-cycle))
  :config
  (setq hs-hide-comments-when-hiding-all nil
        ;; Nicer code-folding overlays (with fringe indicators)
        hs-set-up-overlay #'hideshow-set-up-overlay-fn)

  (defface hideshow-folded-face
    `((t (:inherit font-lock-comment-face :weight light)))
    "Face to hightlight `hideshow' overlays."
    :group 'hideshow)

  (defun hideshow-set-up-overlay-fn (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put
       ov 'display (propertize "  [...]  " 'face 'hideshow-folded-face))))

  (dolist (hs-command (list #'hs-cycle
                            #'hs-global-cycle))
    (advice-add hs-command :before
                (lambda (&optional end) "Advice to ensure `hs-minor-mode' is enabled"
                  (unless (bound-and-true-p hs-minor-mode)
                    (hs-minor-mode +1)))))

  (defun hs-cycle (&optional level)
    (interactive "p")
    (if (= level 1)
        (pcase last-command
          ('hs-cycle
           (hs-hide-level 1)
           (setq this-command 'hs-cycle-children))
          ('hs-cycle-children
           ;;TODO: Fix this case. `hs-show-block' needs to be called twice to
           ;;open all folds of the parent block.
           (save-excursion (hs-show-block))
           (hs-show-block)
           (setq this-command 'hs-cycle-subtree))
          ('hs-cycle-subtree
           (hs-hide-block))
          (_
           (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
      (hs-hide-level level)
      (setq this-command 'hs-hide-level)))

  (defun hs-global-cycle ()
    (interactive)
    (pcase last-command
      ('hs-global-cycle
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))
  )


(use-package ws-butler
  :diminish
  :hook ((prog-mode . ws-butler-mode)))

(use-package blacken)

(use-package dockerfile-mode)
(use-package docker-tramp)

(use-package google-c-style
  :straight (google-c-style
             :type git :host github :repo "google/styleguide")
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  )

;; Java

(use-package google-java-format
  :straight (google-java-format
             :type git :host github :repo "google/google-java-format"
             :files ("core/src/main/scripts/google-java-format.el"))
  :config
  (setq google-java-format-executable (executable-find "google-java-format"))
  )

(defun my/google-java-format-buffer ()
  "Use google-java-format to format the current buffer."
  (interactive)
  (let ((cursor (point))
        (temp-buffer (generate-new-buffer " *google-java-format-temp*"))
        (stderr-file (make-temp-file "google-java-format")))
    (unwind-protect
        (let ((status (call-process-region
                       (point-min) (point-max)
                       google-java-format-executable
                       nil (list temp-buffer stderr-file) t
                       "-"))
              (stderr
               (with-temp-buffer
                 (insert-file-contents stderr-file)
                 (when (> (point-max) (point-min))
                   (insert ": "))
                 (buffer-substring-no-properties
                  (point-min) (line-end-position)))))
          (cond
           ((stringp status)
            (error "google-java-format killed by signal %s%s" status stderr))
           ((not (zerop status))
            (error "google-java-format failed with code %d%s" status stderr))
           (t (message "google-java-format succeeded%s" stderr)
              (replace-buffer-contents temp-buffer)
              ;(goto-char cursor)
              )))
      (delete-file stderr-file)
      (when (buffer-name temp-buffer) (kill-buffer temp-buffer)))))

(add-hook 'java-mode-hook
          (lambda ()
            (setq fill-column 100)
            (add-hook 'before-save-hook #'my/google-java-format-buffer nil t)))

;; C++

(setq clang-format-executable (executable-find "clang-format"))

(defun my/clang-format-buffer ()
  "Use clang-format to format the current buffer."
  (interactive)
  (let ((cursor (point))
        (temp-buffer (generate-new-buffer " *clang-format-temp*"))
        (stderr-file (make-temp-file "clang-format")))
    (unwind-protect
        (let ((status (call-process-region
                       (point-min) (point-max)
                       clang-format-executable
                       nil (list temp-buffer stderr-file) t
                       "--style=file"))
              (stderr
               (with-temp-buffer
                 (insert-file-contents stderr-file)
                 (when (> (point-max) (point-min))
                   (insert ": "))
                 (buffer-substring-no-properties
                  (point-min) (line-end-position)))))
          (cond
           ((stringp status)
            (error "clang-format killed by signal %s%s" status stderr))
           ((not (zerop status))
            (error "clang-format failed with code %d%s" status stderr))
           (t (message "clang-format succeeded%s" stderr)
              (replace-buffer-contents temp-buffer)
                                        ;(goto-char cursor)
              )))
      (delete-file stderr-file)
      (when (buffer-name temp-buffer) (kill-buffer temp-buffer)))))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq fill-column 100)
            (add-hook 'before-save-hook #'my/clang-format-buffer nil t)))

;; Local


;;; Wrap-up

(message "Loading init file...done (%.3fs)"
         (float-time (time-subtract (current-time)
                                    before-user-init-time)))


;;; End of init.el
