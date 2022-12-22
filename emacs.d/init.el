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
         ("C-z" . nil)
         ("C-x C-z" . nil))
  :init
  (setq inhibit-startup-screen t
        initial-scratch-message nil
        sentence-end-double-space nil
        ring-bell-function 'ignore
        frame-resize-pixelwise t
        frame-inhibit-implied-resize t)

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

  ;; don't want ESC as a modifier
  ;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Don't quit w/o warning
  (add-hook 'kill-emacs-query-functions
            (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
            'append)

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
  (auto-save-visited-mode t)
  (add-function :after after-focus-change-function (lambda () (save-some-buffers t)))

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
  (setq show-paren-delay 0
        show-paren-style 'mixed         ; show paren if visible, expr otherwise
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)

  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t)
  ;;(transient-mark-mode -1)
  (put 'narrow-to-region 'disabled nil)

  ;; Automatically set executable bit (chmod) for files with a shebang (#!)
  ;; In practice, this is really annoying for things with #!/hint/bash etc.
  ;;(add-hook 'after-save-hook
  ;;          'executable-make-buffer-file-executable-if-script-p)

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
        native-comp-async-report-warnings-errors nil
        load-prefer-newer t)

  ;; Avoid "ls does not support --dired" message on MacOS
  (when (string= system-type "darwin")
    (require 'ls-lisp)
    (setq ls-lisp-use-insert-directory-program nil))

  (setq dired-dwim-target t)
  (setq windmove-wrap-around t)
  (setq use-dialog-box nil)
  (setq window-min-width 40)
  (setq describe-bindings-outline t)

  ;; Performance: Turn off bidirectional text
  (setq bidi-inhibit-bpa t)
  (setq-default bidi-paragraph-direction 'left-to-right)

  ;; Performance: I/O-related tuning
  (setq process-adaptive-read-buffering nil)
  (setq read-process-output-max (* 1024 1024)) ; 1mb

  ;; Scrolling
  (setq scroll-conservatively 10000
        scroll-margin 2
        scroll-preserve-screen-position nil  ; didn't like t, 1 maybe okay
        comint-terminfo-terminal "ansi"
        comint-scroll-show-maximum-output nil)

  ;; Mode line
  (setq mode-line-compact 'long)
  (display-time-mode -1)
  (column-number-mode t))

;; ;; Tagged buffers
;; ;; as written by Henrik Lissner (author of Doom Emacs)
;; ;; https://discord.com/channels/406534637242810369/806409569013858314/967047606326947841
;; (defvar my/buffer-tags ())

;; (defun my/tag-buffer (n)
;;   (interactive (list (read-number ">")))
;;   (setf (alist-get n my/buffer-tags) (current-buffer)))

;; (defun my/switch-to-tagged-buffer (n)
;;   (interactive (list (read-number ">")))
;;   (switch-to-buffer
;;    (or (alist-get n my/buffer-tags)
;;        (user-error "No buffer with tag %d" n))))

;; (dotimes (i 9)
;;   (global-set-key (kbd (format "C-z C-%d" i))
;;                   (lambda () (interactive)
;;                     (my/tag-buffer i)))
;;   (global-set-key (kbd (format "C-z %d" i))
;;                   (lambda () (interactive)
;;                     (my/switch-to-tagged-buffer i))))


;; Packages

(use-package diminish)

(use-package stutter
  :straight (stutter
             :type git :host github :repo "ark3/stutter.el")
  :custom
  (stutter-minimum-growth 1024 "Smaller chunks useful on MacOS")
  :hook
  (shell-mode . stutter-mode)
  (compilation-mode . stutter-mode))

(use-package hl-line+
  :hook
  (window-scroll-functions . hl-line-flash)
  (focus-in . hl-line-flash)
  (post-command . hl-line-flash)

  :custom
  (global-hl-line-mode nil)
  (hl-line-flash-show-period 0.4)
  (hl-line-inhibit-highlighting-for-modes '(dired-mode vterm-mode))
  (hl-line-overlay-priority -100)) ;; sadly, seems not observed by diredfl

(use-package modus-themes
  :init
  (setq modus-themes-hl-line '(accented)
        modus-themes-completions '((t . (accented intense)))
        modus-themes-mixed-fonts t
        modus-themes-variable-pitch-ui t)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-vivendi))

(use-package avy
  :bind
  (("C-c SPC" . avy-goto-char-timer)
   ("C-'" . avy-goto-word-1)))

(use-package iedit)  ; Binds C-;

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1
        which-key-idle-secondary-delay 0.05
        which-key-show-early-on-C-h nil))      ; Use embark-prefix-help-command

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
  (setq consult-preview-key (kbd "M-.")
        consult-narrow-key "<"))

(use-package consult-dir
  :after (consult vertico)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package wgrep)

(use-package flymake
  :bind (:map flymake-mode-map
              ("C-c f n" . flymake-goto-next-error)
              ("C-c f p" . flymake-goto-prev-error)
              ("C-c f b" . flymake-show-buffer-diagnostics)
              ("C-c f B" . flymake-show-project-diagnostics)))

(use-package vertico
  :bind (:map vertico-map
              ("TAB" . minibuffer-complete)
              ("M-TAB" . vertico-insert))
  :config
  (vertico-mode)
  (setq vertico-resize 'grow-only)

  ;; Use `consult-completion-in-region' if Vertico is enabled, but not in the
  ;; minibuffer. Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if (and vertico-mode (not vertico--input))
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

(use-package fancy-dabbrev
  :diminish fancy-dabbrev-mode
  :bind (("TAB" . fancy-dabbrev-expand-or-indent)
         ("M-/" . dabbrev-completion)
         ("C-M-/" . hippie-expand))
  :config
  (global-fancy-dabbrev-mode)
  (setq fancy-dabbrev-preview-delay 0.3
        fancy-dabbrev-expansion-on-preview-only t))

(use-package ibuffer-project
  :bind ("C-x C-b" . ibuffer)
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (setq ibuffer-filter-groups
                    (ibuffer-project-generate-filter-groups))
              (unless (eq ibuffer-sorting-mode 'project-file-relative)
                (ibuffer-do-sort-by-project-file-relative))))
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-project-use-cache t))

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
        ;; exec-path-from-shell-arguments nil
        exec-path-from-shell-warn-duration-millis 300)
  :config
  (setq ns-function-modifier 'control
        mac-function-modifier 'control
        mac-option-modifier 'meta
        mac-command-modifier 'super)
  (exec-path-from-shell-initialize))

(use-package tramp
  :config
  ;;; include the remote PATH in tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq tramp-histfile-override nil     ; Don't create .tramp_history
        tramp-default-method "scpx")
  ;;; Assume ControlPersist is set in ~/.ssh/config
  (customize-set-variable 'tramp-use-ssh-controlmaster-options nil)

  (defun tramp-ensure-dissected-file-name (vec-or-filename)
    "Return a `tramp-file-name' structure for VEC-OR-FILENAME.

VEC-OR-FILENAME may be either a string or a `tramp-file-name'.
If it's not a Tramp filename, return nil."
    (cond
     ((tramp-file-name-p vec-or-filename) vec-or-filename)
     ((tramp-tramp-file-p vec-or-filename)
      (tramp-dissect-file-name vec-or-filename)))))

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

(defun text-stuff ()
  (setq fill-column 100)
  (setq-local
   dabbrev-case-distinction t     ; treat expansions same if differ in case
   dabbrev-case-fold-search t     ; ignore case on search
   dabbrev-case-replace t)        ; keep typed case
  (visual-fill-column-mode 1)
  (org-indent-mode 1)
  (variable-pitch-mode 1))

(use-package flyspell
  :custom
  (ispell-program-name "aspell"))

(use-package visual-fill-column
  :init
  (add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
  (setq visual-fill-column-center-text t
        visual-fill-column-enable-sensible-window-split t))

(use-package org-autolist
  :after org
  :diminish
  :config
  (setq org-autolist-enable-delete nil))

(use-package org-appear :after org)

(use-package org
  :hook ((org-mode . text-stuff)
         (org-mode . org-appear-mode)
         (org-mode . org-autolist-mode))
  :bind (("C-c x" . org-capture)
         :map org-mode-map
         ("<tab>" . fancy-dabbrev-expand-or-indent)
         ("<backtab>" . fancy-dabbrev-backward))
  :custom
  (org-export-backends '(md ascii html beamer odt latex org))
  (org-hide-emphasis-markers t)
  (org-startup-folded 'content)
  (org-export-with-toc nil)
  (org-export-with-section-numbers nil)
  :config
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "" "Tasks") ; built-in template
           "* TODO %?\n  %u\n  %a")
          ("e" "Email" entry (file "~/temp/email.org")
           "* %?\n")
          ("n" "Note" entry (file+headline "" "Notes")
           "* %?\n\nEntered on %U\n  %i\n  %a"))
        org-default-notes-file (expand-file-name "~/notes.org")
        org-cycle-emulate-tab nil)
  (add-hook 'org-mode-hook
            (lambda () (setq-local fancy-dabbrev-indent-command 'org-cycle))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :hook (markdown-mode . text-stuff)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.mkdn\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))


;;; Programming stuff

(use-package puni
  :hook ((prog-mode sgml-mode nxml-mode tex-mode eval-expression-minibuffer-setup) . puni-mode)
  :config
  (add-hook 'prog-mode-hook 'electric-pair-mode)
  (define-advice puni-kill-line (:before (&rest _))
    "Go back to indentation before killing the line if it makes sense to."
    (when (looking-back "^[[:space:]]*")
      (if (bound-and-true-p indent-line-function)
          (funcall indent-line-function)
        (back-to-indentation))))
  :bind (:map puni-mode-map
              ("C-," . puni-expand-region)))


(defun prog-stuff ()
  (display-line-numbers-mode t)
  (display-fill-column-indicator-mode t)
  (setq fill-column 80
        indent-tabs-mode nil)
  (setq-local
   dabbrev-case-distinction nil    ; different case as different expansions
   dabbrev-case-fold-search t      ; ignore case on search
   dabbrev-case-replace nil))      ; replaced typed case with existing case

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
         :map vterm-copy-mode-map
         ("C-c C-c" . vterm-copy-mode))
  :init
  (setq vterm-always-compile-module t)
  :config
  (setq vterm-max-scrollback 99000
        vterm-tramp-shells '(("docker" "/bin/bash")
                             ("scpx" "/bin/bash")
                             ("sshx" "/bin/bash")))
  (add-to-list 'vterm-eval-cmds '("set" my/term-set-header-message))
  :hook (vterm-mode . my/term-setup))

;; See also other terminal emulators:
;; - https://codeberg.org/akib/emacs-eat (works with eshell)
;; - https://repo.or.cz/emacs-coterm.git (works with shell mode)

(use-package shell
  :config
  (add-hook 'comint-output-filter-functions 'comint-osc-process-output)
  ;;(add-hook 'shell-mode-hook #'my/term-setup)
  (setq comint-terminfo-terminal "ansi"
        comint-scroll-show-maximum-output nil
        comint-input-ignoredups t)
  ;; (map! :map shell-mode-map
  ;;       "M-p" #'comint-previous-matching-input-from-input
  ;;       "M-n" #'comint-next-matching-input-from-input)
  (add-hook 'shell-mode-hook
            (lambda ()
              (my/term-setup)
              (setq-local scroll-margin 0
                          recenter-positions '(top bottom middle)))))

(use-package bash-completion
  :disabled
  :config
  (bash-completion-setup))

;; FIXME: Consider using this instead
;; https://github.com/CeleritasCelery/emacs-native-shell-complete
(use-package native-complete
  :config
  (with-eval-after-load 'shell
    (native-complete-setup-bash))
  (add-hook 'shell-mode-hook
            (lambda ()
              (add-to-list 'completion-at-point-functions #'native-complete-at-point))))

(use-package ansi-color
  :config
  (if (>= emacs-major-version 28)
      (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
    (progn
      (defun colorize-compilation-buffer ()
        (let ((inhibit-read-only t))
          (ansi-color-apply-on-region compilation-filter-start (point))))
      (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))
  (defun display-ansi-colors ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(use-package magit
  :init (setq-default git-magit-status-fullscreen t)
  :bind (("C-c g s" . magit-status)
         ("C-c g g" . magit-file-dispatch)
         ("C-x p m" . magit-project-status))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1  ; fullscreen status
        magit-bury-buffer-function #'magit-restore-window-configuration  ; restore windows on quit
        magit-prefer-remote-upstream t
        project-switch-commands '((project-find-file "Find file")
                                  (project-dired "Dired")
                                  (project-eshell "Eshell")
                                  (project-shell "Shell")
                                  (project-vterm "Vterm")
                                  (magit-project-status "Magit"))))

(use-package git-timemachine
  :disabled ;; not loading for some reason
  :bind (("C-c g t" . git-timemachine)))

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

(use-package eglot-java :after eglot)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"       ; prefix for lsp-command-map
        lsp-idle-delay 0.3              ; default 0.5
        lsp-completion-provider :none   ; don't fuss with company?
        lsp-file-watch-threshold 20000)
  :hook ((python-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . lsp-ui-mode)))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'top
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-sideline-diagnostic-max-lines 10))

(use-package lsp-java
  :bind (:map lsp-command-map
              ("=j" . my/google-java-format-buffer))
  :config
  (defun my/lsp-java-delete-workspace-cache ()
    "Delete the workspace cache so JDTLS has a chance to start successfully."
    (interactive)
    (message "Deleting %s..." lsp-java-workspace-cache-dir)
    (delete-directory lsp-java-workspace-cache-dir t)
    (message "Deleted %s" lsp-java-workspace-cache-dir))
  (setq lsp-java-maven-download-sources t
        lsp-java-format-settings-url "https://raw.githubusercontent.com/google/styleguide/gh-pages/eclipse-java-google-style.xml"
        lsp-java-java-path "java-for-jdt.sh"
        lsp-java-content-provider-preferred "fernflower"))

(use-package eldoc
  :diminish "doc"
  :config
  (setq eldoc-echo-area-use-multiline-p 0.5))

(use-package protobuf-mode)
(use-package go-mode)
(use-package graphql-mode)

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
  :hook (yaml-mode . prog-stuff))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-prefer-searcher 'rg))

(use-package hideshow ; built-in
  ;; https://karthinks.com/software/simple-folding-with-hideshow/
  ;; Roughly https://github.com/karthink/.emacs.d/blob/master/init.el#L3159
  :diminish hs-minor-mode
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
      (_ (hs-hide-all)))))

(use-package ws-butler
  :diminish
  :hook ((prog-mode . ws-butler-mode)))

(use-package dockerfile-mode)
(use-package docker-tramp :disabled)

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
