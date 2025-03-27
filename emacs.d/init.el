;;; init.el -*- lexical-binding: t -*-

;;; Preamble

(message "Loading Emacs, pre-init...done (%ss)" (emacs-init-time "%.4f"))
(message "Loading %s..." user-init-file)


;;; Package management

;; Set up straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(setq straight-use-package-by-default t     ; Always use straight to install
      use-package-always-demand t           ; Don't defer loading by default
      straight-vc-git-default-clone-depth 1) ; Shallow clone

;; Prevent Emacs-provided packages from being loaded
(straight-register-package 'flymake)
(straight-register-package 'org)
(straight-register-package 'org-contrib)
(straight-register-package 'modus-themes)
;;(straight-register-package 'tramp)


;;; General stuff

(use-package emacs
  :bind (("s-<up>" . beginning-of-buffer)
         ("s-<down>" . end-of-buffer)
         ("M-<down>" . scroll-up-command)
         ("M-<up>" . scroll-down-command)
         ("C-<next>" . View-scroll-line-forward)
         ("C-<prior>" . View-scroll-line-backward)
         ("M-SPC" . cycle-spacing)
         ("M-o" . other-window)
         ("M-`" . bury-buffer)
         ("C-x j" . duplicate-dwim)
         ("C-x m" . execute-extended-command)
         ("C-x x r" . rename-visited-file)
         ("C-x C-b" . ibuffer)
         ("C-z" . undo)
         ("C-S-z" . undo-redo)
         ("C-x C-z" . undo-only))
  :init
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        sentence-end-double-space nil
        require-final-newline t
        tab-always-indent 'complete
        show-trailing-whitespace t
        indent-tabs-mode nil
        ring-bell-function 'ignore)

  (setq user-full-name "Abhay Saxena"
        user-mail-address "ark3@email.com")

  ;; always allow 'y' instead of 'yes'.
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; default to utf-8 for all the things FIXME this breaks coterm :frown:
  ;; (set-charset-priority 'unicode)
  ;; (setq locale-coding-system 'utf-8
  ;;       coding-system-for-read 'utf-8
  ;;       coding-system-for-write 'utf-8)
  ;; (set-terminal-coding-system 'utf-8)
  ;; (set-keyboard-coding-system 'utf-8)
  ;; (set-selection-coding-system 'utf-8)
  ;; (prefer-coding-system 'utf-8)
  ;; (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; write over selected text on input... like all modern editors do
  (delete-selection-mode t)

  ;; Don't quit w/o warning
  (setq confirm-kill-emacs 'y-or-n-p)

  (defun keyboard-escape-quit--around (orig-fun &rest args)
    "Don't mess up window layout on keyboard escape quit."
    (let ((buffer-quit-function (lambda () ())))
      (apply orig-fun args)))
  (advice-add 'keyboard-escape-quit :around #'keyboard-escape-quit--around)

  ;; Don't persist a custom file, this bites me more than it helps
  (setq custom-file (make-temp-file "")) ; use a temp file as a placeholder
  (setq custom-safe-themes t)            ; mark all themes as safe, since we can't persist now
  (setq enable-local-variables t)        ; was :all (fix =defvar= warnings)

  ;; stop emacs from littering the file system with backup files
  (setq make-backup-files nil
        auto-save-default nil
        create-lockfiles nil)

  ;; autosave files in-place regularly
  (auto-save-visited-mode t)
  (add-function :after after-focus-change-function (lambda () (save-some-buffers t)))

  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))

  ;; follow symlinks
  (setq vc-follow-symlinks t
        find-file-visit-truename t)

  ;; Buffer/window stuff
  (winner-mode t)   ;; enable winner mode globally for undo/redo window layout changes

  (customize-set-variable 'mouse-wheel-scroll-amount
                          '(1 ((shift) . hscroll) ((meta) . nil))
                          "Turn off mouse-wheel-text-scale")

  (show-paren-mode t)
  (setq show-paren-delay 0
        show-paren-style 'mixed         ; show paren if visible, expr otherwise
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)

  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  (add-to-list 'global-auto-revert-ignore-modes 'Buffer-menu-mode)
  (put 'narrow-to-region 'disabled nil)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
        native-comp-async-report-warnings-errors nil
        load-prefer-newer t)

  ;; Avoid "ls does not support --dired" message on MacOS
  (when (string= system-type "darwin")
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))

  (setq dired-dwim-target t
        dired-kill-when-opening-new-dired-buffer t)
  (setq windmove-wrap-around t)
  (setq use-dialog-box nil)
  (setq window-min-width 40)

  ;; Performance: Turn off bidirectional text
  (setq bidi-inhibit-bpa t)
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Performance: I/O-related tuning
  (setq read-process-output-max (* 1024 1024)) ; 1mb

  ;; Scrolling
  (setq scroll-conservatively 10000
        scroll-margin 2
        scroll-preserve-screen-position nil  ; didn't like t, 1 maybe okay
        scroll-error-top-bottom t            ; move point when can't scroll
        comint-scroll-show-maximum-output nil)
  (setq compilation-scroll-output 'first-error)

  ;; Interactive search
  (setq isearch-lazy-count t
        isearch-lazy-highlight t)

  ;; Mode line
  (setq mode-line-compact 'long)
  (display-time-mode -1)
  (column-number-mode t))

(defun my/maximize-vertically ()
  (interactive)
  (set-frame-height nil (- (display-pixel-height) 96) nil t))


;; Packages

(use-package diminish)

(use-package mood-line
  :config
  (mood-line-mode))

(use-package lin
  :config
  (customize-set-variable 'lin-face 'lin-cyan)
  (global-hl-line-mode 1)
  (lin-global-mode 1))

(use-package modus-themes
  :config
  (setq modus-themes-hl-line '(accented)
        modus-themes-completions '((t . (accented intense)))
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t)
  (load-theme 'modus-vivendi :no-confirm))

(use-package avy
  :bind
  (("C-c SPC" . avy-goto-char-timer)
   ("C-'" . avy-goto-char-timer))
  :config
  ;; https://karthinks.com/software/avy-can-do-anything/#avy-plus-embark-any-action-anywhere
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package iedit)  ; Binds C-;

(use-package which-key
  :config
  (which-key-mode))

;; Use l/r to go back/forward in dired
;; https://github.com/karthink/dired-hist
(use-package dired-hist
  :straight (dired-hist
             :type git :host github :repo "karthink/dired-hist")
  :bind (:map dired-mode-map
              ("l" . dired-hist-go-back)
              ("r" . dired-hist-go-forward))
  :config
  (dired-hist-mode 1))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("s-." . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key "M-."
        consult-narrow-key "<"))

(use-package consult-dir
  :after (consult vertico)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

(use-package key-chord
  :config
  (key-chord-mode t))

(use-package devil
  :disabled
  :straight (devil
             :type git :host github :repo "fbrosda/devil" :branch "dev")
  :config
  (global-devil-mode))

(define-prefix-command 'my/leader-map)
(global-set-key (kbd "C-c") 'my/leader-map)
(global-set-key (kbd "M-m") 'my/leader-map)
(key-chord-define-global "dk" 'my/leader-map)

(define-key my/leader-map (kbd "x") 'execute-extended-command)

(define-prefix-command 'my/app-map)
(define-prefix-command 'my/shell-map)
(define-key my/leader-map (kbd "a") (cons "app" my/app-map))
(define-key my/app-map (kbd "a") 'async-shell-command)
(define-key my/app-map (kbd "s") my/shell-map)

(define-prefix-command 'my/buffer-map)
(define-key my/leader-map (kbd "b") (cons "buffer" my/buffer-map))
(define-key my/buffer-map (kbd "`") 'bury-buffer)
(define-key my/buffer-map (kbd "b") 'consult-buffer)
(define-key my/buffer-map (kbd "B") 'ibuffer)
(define-key my/buffer-map (kbd "d") 'kill-current-buffer)
(define-key my/buffer-map (kbd "k") 'kill-current-buffer)
(define-key my/buffer-map (kbd "v") 'revert-buffer)
(define-key my/buffer-map (kbd "x") 'kill-buffer-and-window)
(define-key my/buffer-map (kbd "s") 'save-buffer)
(define-key my/buffer-map (kbd "S") 'save-some-buffers)

(define-prefix-command 'my/code-map)
(define-prefix-command 'my/lsp-map)
(define-prefix-command 'my/flymake-map)
(define-key my/leader-map (kbd "c") (cons "code" my/code-map))
(define-key my/code-map (kbd "x") 'kill-compilation)
(define-key my/code-map (kbd "l") (cons "lsp" my/lsp-map))
(define-key my/lsp-map (kbd "a") 'lsp-execute-code-action)
(define-key my/lsp-map (kbd "h") 'lsp-describe-thing-at-point)
(define-key my/lsp-map (kbd "o") 'lsp-organize-imports)
(define-key my/lsp-map (kbd "r") 'lsp-rename)
(define-key my/lsp-map (kbd "w") 'lsp-workspace-restart)
(define-key my/lsp-map (kbd "=") 'lsp-format-buffer)
(define-key my/code-map (kbd "f") (cons "flymake" my/flymake-map))
(define-key my/flymake-map (kbd "n") 'flymake-goto-next-error)
(define-key my/flymake-map (kbd "p") 'flymake-goto-prev-error)
(define-key my/flymake-map (kbd "b") 'flymake-show-buffer-diagnostics)
(define-key my/flymake-map (kbd "B") 'flymake-show-project-diagnostics)

(define-prefix-command 'my/file-map)
(define-key my/leader-map (kbd "f") (cons "file" my/file-map))
(define-key my/file-map (kbd "d") 'dired-jump)
(define-key my/file-map (kbd "D") 'dired)
(define-key my/file-map (kbd "f") 'find-file)
(define-key my/file-map (kbd "r") 'consult-recent-file)
(define-key my/file-map (kbd "R") 'rename-visited-file)
(define-key my/file-map (kbd "s") 'save-buffer)
(define-key my/file-map (kbd "v") 'find-alternate-file)
(define-key my/file-map (kbd "w") 'write-file)

(define-prefix-command 'my/go-map)
(define-key my/leader-map (kbd "g") (cons "go" my/go-map))
(define-key my/go-map (kbd "e") 'consult-compile-error)
(define-key my/go-map (kbd "f") 'consult-flymake)
(define-key my/go-map (kbd "g") 'consult-goto-line)
(define-key my/go-map (kbd "o") 'consult-outline)
(define-key my/go-map (kbd "m") 'consult-mark)
(define-key my/go-map (kbd "k") 'consult-global-mark)
(define-key my/go-map (kbd "i") 'consult-imenu)
(define-key my/go-map (kbd "I") 'consult-imenu-multi)

(define-key my/leader-map (kbd "h") (cons "help" help-map))

(define-prefix-command 'my/jump-map)
(define-key my/leader-map (kbd "j") (cons "jump" my/jump-map))
(define-key my/jump-map (kbd "j") 'avy-goto-char-timer)
(define-key my/jump-map (kbd "r") 'jump-to-register)

(define-key my/leader-map (kbd "p") (cons "project" project-prefix-map))

(define-prefix-command 'my/quit-map)
(define-key my/leader-map (kbd "q") (cons "quit" my/quit-map))
(define-key my/quit-map (kbd "d") 'restart-emacs-debug-init)
(define-key my/quit-map (kbd "r") 'restart-emacs)
(define-key my/quit-map (kbd "R") 'restart-emacs-without-desktop)
(define-key my/quit-map (kbd "f") 'delete-frame)
(define-key my/quit-map (kbd "q") 'save-buffers-kill-terminal)
(define-key my/quit-map (kbd "Q") 'save-buffers-kill-emacs)

(define-prefix-command 'my/search-map)
(define-key my/leader-map (kbd "s") (cons "search" my/search-map))
(define-key my/search-map (kbd "f") 'consult-find)
(define-key my/search-map (kbd "F") 'consult-locate)
(define-key my/search-map (kbd "g") 'consult-grep)
(define-key my/search-map (kbd "G") 'consult-git-grep)
(define-key my/search-map (kbd "r") 'consult-ripgrep)
(define-key my/search-map (kbd "h") 'consult-history)
(define-key my/search-map (kbd "l") 'consult-line)
(define-key my/search-map (kbd "L") 'consult-line-multi)
(define-key my/search-map (kbd "k") 'consult-keep-lines)
(define-key my/search-map (kbd "u") 'consult-focus-lines)
(define-key my/search-map (kbd "e") 'consult-isearch-history)
(define-key my/search-map (kbd "s") 'save-buffer)

(define-prefix-command 'my/toggle-map)
(define-key my/leader-map (kbd "t") (cons "toggle" my/toggle-map))
(define-key my/toggle-map (kbd "r") 'read-only-mode)
(define-key my/toggle-map (kbd "t") 'toggle-truncate-lines)
(define-key my/toggle-map (kbd "v") 'visual-line-mode)
(define-key my/toggle-map (kbd "V") 'visual-fill-column-mode)
(define-key my/toggle-map (kbd "w") 'whitespace-mode)

(define-prefix-command 'my/window-map)
(define-key my/leader-map (kbd "w") (cons "window" my/window-map))
(define-key my/window-map (kbd "=") 'window-swap-states)
(define-key my/window-map (kbd "0") 'delete-window)
(define-key my/window-map (kbd "1") 'delete-other-windows)
(define-key my/window-map (kbd "2") 'split-window-below)
(define-key my/window-map (kbd "3") 'split-window-right)
(define-key my/window-map (kbd "4") ctl-x-4-map)
(define-key my/window-map (kbd "5") ctl-x-5-map)
(define-key my/window-map (kbd "o") 'other-window-prefix)
(define-key my/window-map (kbd "t") 'toggle-window-split)
(define-key my/window-map (kbd "u") 'winner-undo)
(define-key my/window-map (kbd "U") 'winner-redo)
(define-key my/window-map (kbd "v") 'my/maximize-vertically)
(define-key my/window-map (kbd "w") 'ace-window)

(use-package flymake
  :bind (:map flymake-mode-map
              ("C-c f n" . flymake-goto-next-error)
              ("C-c f p" . flymake-goto-prev-error)
              ("C-c f b" . flymake-show-buffer-diagnostics)
              ("C-c f B" . flymake-show-project-diagnostics))
  :config
  (setq flymake-show-diagnostics-at-end-of-line nil))

(use-package vertico
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char)
              ("TAB" . minibuffer-complete)
              ("M-TAB" . vertico-insert))
  :config
  (vertico-mode)
  (setq vertico-resize 'grow-only))

(use-package cape
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package corfu
  :config
  (setq corfu-auto t
        corfu-quit-no-match t)
  (define-key corfu-map (kbd "RET") nil)
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode))

(use-package orderless
  :init
  (setq orderless-matching-styles
        '(orderless-literal orderless-regexp orderless-prefixes))
  (setq completion-styles (append completion-styles '(orderless))
        ;;completion-category-overrides '((file (styles basic partial-completion)))
        completion-category-defaults nil))

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

  ;; Add prompt indicator to `completing-read-multiple'. Propertize the
  ;; separator to make it stand out.
  (defun crm-indicator (args)
    (cons (format "[Multi: %s] %s"
                  (propertize
                   (replace-regexp-in-string
                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                    crm-separator)
                   'face 'error)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t))

(use-package edit-server
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t)
  :config (setq edit-server-new-frame nil))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :init
  (setq exec-path-from-shell-variables '("PATH" "JAVA_HOME")
        exec-path-from-shell-arguments '("-l")
        ;; exec-path-from-shell-debug t
        exec-path-from-shell-warn-duration-millis 100)
  :config
  (setq ns-function-modifier 'control
        mac-function-modifier 'control
        mac-option-modifier 'meta
        mac-command-modifier 'super)
  (exec-path-from-shell-initialize))

(use-package tramp
  :straight nil
  :config
  ;;; include the remote PATH in tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-histfile-override nil     ; Don't create .tramp_history
        tramp-default-method "scpx"
        tramp-copy-size-limit nil
        vc-handled-backends '(Git))
  ;;; Assume ControlPersist is set in ~/.ssh/config
  (customize-set-variable 'tramp-use-ssh-controlmaster-options nil))

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
         ("C" . #'helpful-command))
  :config
  ;; FIXME: Use display-buffer-alist to do this somehow?
  (setq helpful-switch-buffer-function #'my/helpful-switch-to-buffer)
  (defun my/helpful-switch-to-buffer (buffer-or-name)
    (if (eq major-mode 'helpful-mode)
        (switch-to-buffer buffer-or-name)
      (pop-to-buffer buffer-or-name))))


;;; Text stuff

(use-package qrencode
  :bind (:map my/app-map ("q" . qrencode-region)))

(use-package flyspell
  :custom
  (ispell-program-name "aspell"))

(use-package visual-fill-column
  :init
  (add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
  (setq-default fill-column 100)
  (setq visual-fill-column-center-text t
        visual-fill-column-enable-sensible-window-split t))

(use-package org-autolist
  :after org
  :diminish
  :config
  (setq org-autolist-enable-delete nil))

(use-package org-appear :after org)

(use-package org
  :hook ((org-mode . visual-fill-column-mode)
         (org-mode . org-appear-mode)
         (org-mode . org-autolist-mode))
  :bind (:map my/app-map ("x" . org-capture))
  :custom
  (org-export-backends '(md ascii html beamer odt latex org))
  (org-hide-emphasis-markers t)
  (org-startup-folded 'content)
  (org-startup-indented t)
  (org-support-shift-select 'always)
  (org-export-with-toc nil)
  (org-export-with-section-numbers nil)
  (org-export-initial-scope 'subtree)
  :config
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "" "Tasks") ; built-in template
           "* TODO %?\n  %u\n  %a")
          ("e" "Email" entry (file+headline "" "Emails")
           "* %?\n")
          ("n" "Note" entry (file+headline "" "Notes")
           "* %?\n\nEntered on %U\n  %i\n  %a"))
        org-default-notes-file (expand-file-name "~/notes.org")
        org-cycle-emulate-tab nil)
  )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :hook (markdown-mode . visual-fill-column-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mkdn\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc")
  :config (setq markdown-fontify-code-blocks-natively t))

;;; Programming stuff

(use-package expand-region
  :bind (("C-," . er/expand-region)     ; overridden by puni in prog modes
         ("C-=" . er/expand-region)))

(use-package puni
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  :config
  (add-hook 'prog-mode-hook 'electric-pair-mode)
  :bind (:map puni-mode-map
              ("C-," . puni-expand-region))) ; Overrides er/expand-region


(defun prog-stuff ()
  (display-line-numbers-mode t)
  (display-fill-column-indicator-mode t)
  (setq fill-column 80)
  )

(add-hook 'prog-mode-hook 'prog-stuff)

(use-package aggressive-indent
  :hook ((emacs-lisp-mode . aggressive-indent-mode)))

(defun my/term-set-header-message (message)
  (setq-local my/term-header-message (base64-decode-string (or message ""))))

(defun my/term-setup ()
  (my/term-set-header-message (base64-encode-string ""))
  (setq header-line-format
        '(" "
          (:eval (ansi-color-apply my/term-header-message))
          (:eval (string-trim (abbreviate-file-name default-directory) "" "/")))))

(defun project-vterm ()
  "Invoke `vterm' in the project's root.
Switch to the project specific term buffer if it already exists."
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (project-vterm-name (project-prefixed-buffer-name "vterm")))
    (unless (buffer-live-p (get-buffer project-vterm-name))
      (vterm project-vterm-name)
      (vterm-send-string (concat "cd " default-directory))
      (vterm-send-return))
    (pop-to-buffer-same-window (get-buffer project-vterm-name))))

(use-package vterm
  :bind (
         :map project-prefix-map
         ("v" . project-vterm)
         :map my/app-map
         ("v" . vterm)
         :map vterm-copy-mode-map
         ("C-c C-c" . vterm-copy-mode)
         :map vterm-mode-map
         ("C-v" . vterm-yank)
         ("C-c C-v" . vterm-yank))
  :init
  (setq vterm-always-compile-module t)
  :config
  (setq vterm-max-scrollback 99000
        vterm-tramp-shells '(("docker" "/bin/bash")
                             ("scpx" "/bin/bash")
                             ("sshx" "/bin/bash")))
  (add-to-list 'vterm-eval-cmds '("set" my/term-set-header-message))
  :hook (vterm-mode . my/term-setup))

(use-package eat                        ; https://codeberg.org/akib/emacs-eat
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

(use-package coterm                     ; https://repo.or.cz/emacs-coterm.git
  :bind (:map shell-mode-map
              ("C-c C-j" . coterm-char-mode-cycle)
              ("C-c C-k" . coterm-char-mode-cycle))
  :config
  (coterm-mode))

;; async-shell-command: when the process is done
;; - switch to view-mode (for q to quit)
;; - set things up so revert re-runs the command
;; - bind g to revert-buffer
;; see
;; - https://emacs.stackexchange.com/a/41418 (sentinel for done, view mode)
;; - https://emacs.stackexchange.com/a/35638 (set up revert)

;; https://github.com/elizagamedev/shell-command-x.el
(use-package shell-command-x
  :custom
  (shell-command-x-buffer-name-format "*cmd:%n*" "not really shell-related")
  (shell-command-x-buffer-name-async-format "*cmd:%n*" "not really shell-related")
  :config
  (shell-command-x-mode 1))

(use-package shell
  :bind (:map shell-mode-map
              ("M-P" . comint-previous-matching-input-from-input)
              ("M-N" . comint-next-matching-input-from-input))
  :config
  (defun my/make-shell-in-dir (dir &optional buffer-base shell-file-name)
    (let* ((name (or buffer-base (file-name-nondirectory dir)))
           (function-name (intern (concat "shell-in-" name))))
      (fset function-name
            (lambda ()
              (:documentation
               (format "Create a shell in %s.\n\nRun the shell %s in the directory %s"
                       name shell-file-name dir))
              (interactive)
              (let* ((default-directory dir)
                     (buffer-name (format "*shell-%s*"
                                          name))
                     (buffer (get-buffer-create buffer-name)))
                (shell buffer shell-file-name))))
      function-name))
  (define-key my/shell-map (kbd "h") (my/make-shell-in-dir (getenv "HOME")))
  (define-key my/shell-map (kbd "s")
              (my/make-shell-in-dir "/scpx:strife:" "strife" "/bin/bash"))

  (defun my/cursor-at-shell-prompt-p ()
    "Return non-nil if the cursor is at or after the shell prompt."
    (>= (point) (marker-position (process-mark (get-buffer-process (current-buffer))))))
  (defun my/shell-up-key (n)
    "Move up in the history if at the shell prompt, otherwise move the cursor up."
    (interactive "p")
    (if (my/cursor-at-shell-prompt-p)
        (let ((last-command (if (memq last-command '(my/shell-up-key my/shell-down-key))
                                'comint-previous-matching-input-from-input
                              last-command)))
          (comint-previous-matching-input-from-input n))
      (previous-line n)))
  (defun my/shell-down-key (n)
    "Move down in the history if at the shell prompt, otherwise move the cursor down."
    (interactive "p")
    (if (my/cursor-at-shell-prompt-p)
        (let ((last-command (if (memq last-command '(my/shell-up-key my/shell-down-key))
                                'comint-previous-matching-input-from-input
                              last-command)))
          (comint-next-matching-input-from-input n))
      (next-line n)))
  (with-eval-after-load 'shell
    (define-key shell-mode-map (kbd "<up>") 'my/shell-up-key)
    (define-key shell-mode-map (kbd "<down>") 'my/shell-down-key))

  (setq comint-terminfo-terminal "ansi"
        comint-scroll-show-maximum-output nil ; to preserve C-l recentering
        comint-move-point-for-output nil
        comint-scroll-to-bottom-on-input t
        comint-prompt-read-only t
        comint-completion-addsuffix t
        comint-input-ignoredups t)
  (add-hook 'comint-output-filter-functions 'comint-osc-process-output)
  (add-hook 'shell-mode-hook
            (lambda ()
              (my/term-setup)
              (abbrev-mode 1)
              (setq-local scroll-margin 0
                          recenter-positions '(top bottom middle)))))

(use-package ansi-color
  :config
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
  (defun display-ansi-colors ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package magit
  :init (setq-default git-magit-status-fullscreen t)
  :bind (
         :map my/app-map
         ("g" . magit-file-dispatch)
         :map project-prefix-map
         ("m" . magit-project-status))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1  ; fullscreen status
        magit-bury-buffer-function #'magit-restore-window-configuration  ; restore windows on quit
        magit-prefer-remote-upstream t
        magit-diff-refine-hunk t
        project-switch-commands '((project-find-file "Find file")
                                  (project-dired "Dired")
                                  (project-eshell "Eshell")
                                  (project-shell "Shell")
                                  (project-vterm "Vterm")
                                  (magit-project-status "Magit"))))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (unless (window-system) (diff-hl-margin-mode))
  (diff-hl-flydiff-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  (unbind-key "<tab>" yas-minor-mode-map)
  (unbind-key "TAB" yas-minor-mode-map))

(use-package yasnippet-snippets
  :disabled
  :after yasnippet)

(use-package eglot
  :disabled
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l =" . eglot-format-buffer)
              ("C-c l R" . eglot-reconnect)
              ("C-c l K" . eglot-shutdown-all))
  :hook ((python-mode . eglot-ensure)
         (c++-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (go-mode . eglot-ensure))
  :config
  (set-face-attribute 'eglot-highlight-symbol-face nil
                      :inherit 'match))

(use-package eglot-java
  :disabled
  :after eglot)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"       ; prefix for lsp-command-map
        lsp-idle-delay 0.3              ; default 0.5
        lsp-completion-provider :none   ; don't fuss with company?
        lsp-keep-workspace-alive nil
        lsp-file-watch-threshold 20000)
  :hook ((python-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (scala-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-lens-mode)
         (lsp-mode . lsp-ui-mode)))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-diagnostic-max-lines 10))

(use-package lsp-java
  :config
  (defun my/lsp-java-delete-workspace-cache ()
    "Delete the workspace cache so JDTLS has a chance to start successfully."
    (interactive)
    (message "Deleting %s..." lsp-java-workspace-cache-dir)
    (delete-directory lsp-java-workspace-cache-dir t)
    (message "Deleted %s" lsp-java-workspace-cache-dir))
  (let* ((lombok (substitute-in-file-name "$HOME/.m2/repository/org/projectlombok/lombok/1.18.30/lombok-1.18.30.jar"))
         (extra-arg (concat "-javaagent:" lombok)))
    (add-to-list 'lsp-java-vmargs extra-arg))
  (setq lsp-java-maven-download-sources t
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        ;; lsp-java-java-path "java-for-jdt.sh"
        lsp-java-compile-null-analysis-mode "automatic"
        lsp-java-content-provider-preferred "fernflower"))

(use-package lsp-jedi)

(use-package eldoc
  :diminish "doc"
  :config
  (setq eldoc-echo-area-use-multiline-p 0.5))

(use-package protobuf-mode)
(use-package go-mode)
(use-package graphql-mode)

(use-package scala-mode
  :interpreter ("scala" . scala-mode))
(use-package lsp-metals)

(use-package sbt-mode
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package tree-sitter-langs
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter
  :diminish
  :config
  (global-tree-sitter-mode)
  :after (tree-sitter-langs))

(use-package cmake-mode)

(use-package yaml-mode
  :hook ((yaml-mode . prog-stuff)
         (yaml-mode . apheleia-mode)))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg))

(use-package ws-butler
  :diminish
  :hook ((prog-mode . ws-butler-mode)))

(use-package dockerfile-mode)

(use-package apheleia
  :config
  (setq apheleia-mode-lighter " fmt"
        apheleia-remote-algorithm 'remote)
  (push '(elisp-mode . lisp-indent) apheleia-mode-alist)
  :hook
  (prog-mode . apheleia-mode))

(use-package google-c-style
  :hook
  (java-mode . google-set-c-style))

(add-hook 'java-mode-hook (lambda () (setq fill-column 100)))
(add-hook 'c++-mode-hook (lambda () (setq fill-column 100)))

(use-package gptel
  :config
  (setq
   gptel-model "llama3:latest"
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models '("gemma:7b" "llama3:latest" "codegemma:latest"
                             "codellama:latest" "zephyr:latest")))
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  )

(use-package mu4e
  :disabled
  :config
  (setq
   mu4e-search-skip-duplicates  t
   mu4e-view-show-images t
   mu4e-view-show-addresses t
   mu4e-compose-format-flowed nil
   mu4e-date-format "%y-%m-%d"
   mu4e-headers-date-format "%Y-%m-%d"
   mu4e-change-filenames-when-moving t
   mu4e-attachments-dir "~/Downloads"

   mu4e-maildir       "~/Maildir"   ;; top-level Maildir
   ;; note that these folders below must start with /
   ;; the paths are relative to maildir root
   mu4e-refile-folder "/Archive"
   mu4e-sent-folder   "/Sent"
   mu4e-drafts-folder "/Drafts"
   mu4e-trash-folder  "/Trash")

  ;; this setting allows to re-sync and re-index mail
  ;; by pressing U
  (setq mu4e-get-mail-command  "mbsync -a")
  (fset 'my-move-to-trash "mTrash")
  (define-key mu4e-headers-mode-map (kbd "d") 'my-move-to-trash)
  (define-key mu4e-view-mode-map (kbd "d") 'my-move-to-trash)
  )

;; WSL-specific setup

(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
  ;; WSL clipboard fix
  (setq select-active-regions nil
        select-enable-clipboard 't
        select-enable-primary nil
        interprogram-cut-function #'gui-select-text)

  ;; Open links in the default Windows browser
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser  'browse-url-generic))))

;; direnv support using https://github.com/purcell/envrc
;;
;; It's probably wise to do this late in your startup sequence: you normally
;; want envrc-mode to be initialized in each buffer before other minor modes
;; like flycheck-mode which might look for executables. Counter-intuitively,
;; this means that envrc-global-mode should be enabled after other global minor
;; modes, since each prepends itself to various hooks.
(use-package envrc
  :if (executable-find "direnv")
  :bind (:map envrc-mode-map
              ("C-c E" . envrc-command-map))
  :config
  (setq envrc-none-lighter
        '(" env[" (:propertize "-" face envrc-mode-line-none-face) "]")
        envrc-on-lighter
        '(" env[" (:propertize "+" face envrc-mode-line-on-face) "]")
        envrc-error-lighter
        '(" env[" (:propertize "!" face envrc-mode-line-error-face) "]"))
  (envrc-global-mode))

;; Local

(load "~/.local/emacs/config" t)  ; NOERROR if file does not exist

;;; Wrap-up

;; Save state between sessions
(setq recentf-exclude `(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"
                        ,(expand-file-name "straight/build/" user-emacs-directory)
                        ,(expand-file-name "eln-cache/" user-emacs-directory)
                        ,(expand-file-name "etc/" user-emacs-directory)
                        ,(expand-file-name "var/" user-emacs-directory))
      recentf-max-saved-items 250

      desktop-restore-frames nil        ; don't restore frame configuration
      desktop-restore-eager t           ; restore all buffers immediately

      history-length 10000
      history-delete-duplicates t
      savehist-save-minibuffer-history t)

(desktop-save-mode 1)
(savehist-mode 1)                       ; minibuffer history
(recentf-mode 1)                        ; recently-edited files
(save-place-mode 1)                     ; cursor location in visited files
(dolist (symbol '(kill-ring log-edit-comment-ring))
  (add-to-list 'desktop-globals-to-save symbol))

(message "Loading init file...done (%ss)" (emacs-init-time "%.4f"))

;;; End of init.el
