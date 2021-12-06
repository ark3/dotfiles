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
	frame-resize-pixelwise t)

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
  (recentf-mode 1)
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
  (savehist-mode 1)
  (save-place-mode 1)
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

  ;; don't show any extra window chrome
  (when (window-system)
    (setq-default line-spacing 1)
    ;;(setq my/fixed-face "Source Code Pro")
    (setq my/fixed-face "Iosevka Term SS09")
    ;;(setq my/fixed-face "JetBrainsMono Nerd Font")
    (set-face-attribute 'default nil :font my/fixed-face :height 120 :width 'expanded :weight 'light)
    (set-face-attribute 'fixed-pitch nil :font my/fixed-face :height 120 :width 'expanded :weight 'light)
    ;;(set-face-attribute 'fixed-pitch nil :font my/fixed-face :height 130)
    ;;(set-face-attribute 'variable-pitch nil :font "ia Writer Duospace" :height 130)
    ;;(set-face-attribute 'variable-pitch nil :font "PT Sans Caption" :height 130)
    (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height 130 :weight 'light)
    (tool-bar-mode -1)
    (toggle-scroll-bar -1))

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
  (global-set-key (kbd "C-x -") #'toggle-window-split)
  (global-set-key (kbd "C-x C--") #'windmove-swap-states-left)

  (customize-set-variable 'mouse-wheel-scroll-amount
			  '(1 ((shift) . hscroll) ((meta) . nil))
			  "Turn off mouse-wheel-text-scale")

  (show-paren-mode t)
  (global-auto-revert-mode 1)
  (transient-mark-mode -1)
  (put 'narrow-to-region 'disabled nil)
  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
	native-comp-async-report-warnings-errors nil
	load-prefer-newer t
	)

  ;; Tramp
  (require 'tramp)
  ;;; include the remote PATH in tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  ;;; Don't create .tramp_history
  (setq tramp-histfile-override nil)

  ;; Avoid "ls does not support --dired" message on MacOS
  (when (string= system-type "darwin")
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))

  (setq dired-dwim-target t)
  (setq windmove-wrap-around t)

  ;; Scrolling
  (setq scroll-conservatively 10000
	comint-scroll-show-maximum-output nil)

  ;; Mode line
  (display-time-mode -1)
  (column-number-mode t)
  )

(use-package diminish)

;; https://github.com/bbatsov/super-save
(use-package super-save
  :diminish super-save-mode
  :config
  (setq super-save-remote-files nil
	super-save-auto-save-when-idle nil)
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
  (setq modus-themes-hl-line '(intense accented))
  :config (load-theme 'modus-vivendi))

(use-package avy
  :bind
  (("C-;" . avy-goto-char-timer)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1
	which-key-idle-secondary-delay 0.05
	which-key-show-early-on-C-h t)
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

(use-package consult
  :bind (
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-comman
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)
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

(use-package vertico
  :config
  (vertico-mode)
  (setq vertico-resize nil) ; Don't grow and shrink the Vertico minibuffer

  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
	(lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
		 args)))
  )

(use-package corfu
  :disabled
  :config
  (corfu-global-mode)
  (setq-default tab-always-indent 'complete
		tab-first-completion 'word-or-paren-or-punct
  )
)

(use-package dabbrev
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (setq dabbrev-case-distinction nil
	dabbrev-case-fold-search t
	dabbrev-case-replace nil)
)

(use-package bufler
  :bind (;;("C-x b" . bufler-switch-buffer) ;; didn't really enjoy this
	 ("C-x C-b" . bufler-list))
  )


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
  :disabled
  :config
  (global-fancy-dabbrev-mode)
  (setq fancy-dabbrev-preview-delay 0.3
	dabbrev-case-distinction nil
	dabbrev-case-fold-search t
	dabbrev-case-replace nil)
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
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

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
	exec-path-from-shell-warn-duration-millis 1500)
  :config
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
    (setq fill-column 80)))

(defun my/vterm-copy-mode-cancel ()
  "Exit vterm-copy-mode without copying anything"
  (interactive)
  (vterm-copy-mode -1))

(defun project-vterm ()
  "Invoke `vterm' in the project's root.
Switch to the project specific term buffer if it already exists."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
	 (project-vterm-name (project-prefixed-buffer-name "vterm")))
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
	 ("C-c C-c" . my/vterm-copy-mode-cancel)
	 )
  :config (setq vterm-max-scrollback 99000)
  )

(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
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

(use-package git-gutter
  :init
  :config
  (setq git-gutter:disabled-modes '(org-mode asm-mode image-mode)
	git-gutter:update-interval 1
	git-gutter:window-width 2
	git-gutter:ask-p nil)
  (global-git-gutter-mode)
  )

(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :after git-gutter
  :demand fringe-helper
  :config
  ;; subtle diff indicators in the fringe
  ;; places the git gutter outside the margins.
  (setq-default fringes-outside-margins t)
  ;; thin fringe bitmaps
  (define-fringe-bitmap 'git-gutter-fr:added
  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:modified
  [224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224 224]
  nil nil 'center)
  (define-fringe-bitmap 'git-gutter-fr:deleted
  [0 0 0 0 0 0 0 0 0 0 0 0 0 128 192 224 240 248]
  nil nil 'center))

(use-package flycheck)

(use-package consult-flycheck
  :after (consult flycheck))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"	; prefix for lsp-command-map
	read-process-output-max (* 1024 1024) ; 1mb
	lsp-idle-delay 0.5		      ; default 0.5
	lsp-file-watch-threshold 20000
	lsp-completion-provider :none
	)
  :hook ((python-mode . lsp-deferred)
	 (java-mode . lsp-deferred)
	 (go-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . lsp-ui-mode))
)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package lsp-java
  :config
  (setq lsp-java-maven-download-sources t
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
	lsp-java-vmargs	(list
			 "-XX:+UseParallelGC"
			 "-XX:GCTimeRatio=4"
			 "-XX:AdaptiveSizePolicyWeight=90"
			 "-Dsun.zip.disableMemoryMapping=true"
			 "-noverify"
			 (substitute-in-file-name
			  "--class-path=$HOME/.m2/repository/javax/annotation/javax.annotation-api/1.3.2")
			 (substitute-in-file-name
			  "-javaagent:$HOME/.m2/repository/org/projectlombok/lombok/1.18.16/lombok-1.18.16.jar"))
	lsp-java-content-provider-preferred "fernflower")
  )

(use-package go-mode)
(use-package lsp-pyright)
;  :hook (python-mode . (lambda ()
;                          (require 'lsp-pyright)
;                          (lsp))))

(use-package yaml-mode
  :bind ("C-<tab>" . outline-cycle)
  :hook ((yaml-mode . outline-minor-mode)
	 (yaml-mode . lsp-mode)
	 (yaml-mode . (lambda ()  (progn (setq outline-regexp "^ *##")))))
  )

(use-package ws-butler
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

(use-package google-java-format
  :straight (google-java-format
	     :type git :host github :repo "google/google-java-format"
	     :files ("core/src/main/scripts/google-java-format.el"))
  :config
  (setq google-java-format-executable (executable-find "google-java-format"))
  )

;;; Wrap-up

;; Restore garbage collection to a reasonable value.
;; This is step 2, step 1 is in early-init.
;; In my case this saves about .3 seconds in startup time.
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216 ; 16mb
	  gc-cons-percentage 0.1)))

(message "Loading init file...done (%.3fs)"
	 (float-time (time-subtract (current-time)
				    before-user-init-time)))


;;; End of init.el