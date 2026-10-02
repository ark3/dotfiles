(use-package agent-shell
  :config
  (setopt agent-shell-session-strategy 'new ; display shell buffer immediately
                                        ; to avoid hiding error conditions.
          agent-shell-command-prefix '("sbox")
          agent-shell-permission-responder-function #'agent-shell-permission-allow-always
          ;; D-Bus denies the sleep inhibit in this environment
          ;; (org.freedesktop.DBus.Error.AccessDenied), and agent-shell retries
          ;; it ~100 times per turn, flooding *Messages*.  The inhibit never
          ;; took effect, so nothing is lost by declining to ask.
          agent-shell-inhibit-system-sleep nil)

  ;; Read agent prose in a proportional font, keeping code and anything
  ;; column-aligned monospace.  `variable-pitch-mode' remaps `default'
  ;; buffer-locally, and nearly every agent-shell face inherits from an
  ;; `org-*'/`font-lock-*' face that specifies no :family, so they all follow
  ;; that remap unless pinned.  `fixed-pitch' specifies only :family, so
  ;; prepending it to a face's inheritance supplies the family and leaves
  ;; every other attribute to fall through exactly as before.
  (require 'diff-mode)                  ; for the diff-* faces below

  (defun my/face-prepend-fixed-pitch (face)
    "Keep FACE monospace by prepending `fixed-pitch' to its inheritance."
    (let ((parents (face-attribute face :inherit)))
      (set-face-attribute
       face nil
       :inherit (cons 'fixed-pitch
                      (cond ((eq parents 'unspecified) nil)
                            ((listp parents) parents)
                            (t (list parents)))))))

  ;; `agent-shell-markdown-table' carries no attributes of its own and is
  ;; listed last in the -header/-border/-zebra faces' :inherit, so pinning it
  ;; alone covers every character of a table -- upstream built it for exactly
  ;; this.  The diff faces are the opposite case:
  ;; `agent-shell--format-diff-as-text' applies them directly rather than
  ;; through an agent-shell-* face, so they have to be pinned globally.
  ;;
  ;; `agent-shell-markdown-list-marker' is deliberately absent: pinning it put
  ;; the bullet in a different font from the item text it introduces.
  (dolist (face '(agent-shell-markdown-source-block
                  agent-shell-markdown-inline-code
                  agent-shell-markdown-table
                  diff-added
                  diff-removed
                  diff-hunk-header))
    (my/face-prepend-fixed-pitch face))

  ;; Bigger prose than the global `variable-pitch', with tighter leading to pay
  ;; for it -- which is how the browser rendering fits a larger face into the
  ;; same area.  `variable-pitch-mode' is just `buffer-face-mode' with the
  ;; `variable-pitch' face, so applying a face derived from it via
  ;; `buffer-face-set' keeps the size bump buffer-local; editing
  ;; `variable-pitch' itself would resize every other prose buffer too.  The
  ;; relative :height resolves against the inherited absolute one, so 1.15 over
  ;; `variable-pitch's 100 lands at 114 and still tracks that if it changes.
  (defface my/agent-shell-prose
    '((t :inherit variable-pitch :height 1.15))
    "Face for prose in an agent-shell buffer."
    :group 'agent-shell-faces)

  (defun my/agent-shell-setup-appearance ()
    "Set up per-buffer appearance tweaks for an agent-shell buffer.
Remaps `default' to `my/agent-shell-prose', and sets the leading that
face wants -- neither agent-shell nor shell-maker sets `line-spacing'
anywhere.  Then the list fixups below, added buffer-locally the way
agent-shell adds its own members to these hooks -- added globally they
would also fire in any other shell-maker shell."
    (buffer-face-set 'my/agent-shell-prose)
    (setq-local line-spacing 0.15)
    (add-hook 'shell-maker-finish-output-hook
              #'my/agent-shell-hang-list-indents nil t)
    (add-hook 'agent-shell-ui-post-expand-fragment-at-point-hook
              #'my/agent-shell-hang-list-indents nil t))

  (add-hook 'agent-shell-mode-hook #'my/agent-shell-setup-appearance)

  ;; Hanging indent for wrapped list items.
  ;;
  ;; `agent-shell-markdown--render-list-line' sets only `line-prefix' on a
  ;; rendered list line, never `wrap-prefix'.  `shell-maker--initialize' forces
  ;; `visual-line-mode' on, so a list item longer than the window soft-wraps
  ;; and its continuation lines get no indent at all -- they start at column
  ;; zero, left of even the bullet.  That is upstream's bug and shows up in a
  ;; monospace buffer too; a proportional font only makes it more obvious.
  ;;
  ;; Measure each item's rendered prefix and stretch a `wrap-prefix' to the
  ;; same offset, so wrapped text hangs under the item's first character.
  ;; `string-pixel-width' is given the buffer so it inherits
  ;; `face-remapping-alist' -- without that a variable-pitch buffer measures at
  ;; its unremapped width.  Pixels also mean the indent tracks the font rather
  ;; than assuming a character grid, which a string of spaces cannot do here.
  (defun my/agent-shell--list-marker-p (face)
    "Non-nil if FACE includes `agent-shell-markdown-list-marker'."
    (memq 'agent-shell-markdown-list-marker
          (if (listp face) face (list face))))

  (defun my/agent-shell--list-content-start (bol eol)
    "Return where a rendered list item's text starts on the line at BOL.
Returns nil unless the line really is a rendered item, i.e. leading
indent followed by a marker glyph faced
`agent-shell-markdown-list-marker'.  EOL bounds the search."
    (save-excursion
      (goto-char bol)
      (skip-chars-forward " \t" eol)
      (when (my/agent-shell--list-marker-p (get-text-property (point) 'face))
        (while (and (< (point) eol)
                    (my/agent-shell--list-marker-p
                     (get-text-property (point) 'face)))
          (forward-char 1))
        (skip-chars-forward " \t" eol)
        (point))))

  (defun my/agent-shell-hang-list-indents (&optional beg end)
    "Give rendered list lines between BEG and END a hanging `wrap-prefix'.
Defaults to the accessible portion, so it does the right thing when
called with the buffer narrowed to one fragment body.  Idempotent:
lines that already carry a `wrap-prefix' are skipped."
    (save-excursion
      (let ((end (or end (point-max)))
            ;; A bare string, which is what upstream's default is.  Measured as
            ;; text rather than left on the copy as a `line-prefix' property,
            ;; whose contribution to `string-pixel-width' is not defined.
            (base (if (stringp agent-shell-markdown-list-line-prefix)
                      agent-shell-markdown-list-line-prefix
                    "")))
        (goto-char (or beg (point-min)))
        (while (< (point) end)
          (let ((bol (point))
                (eol (line-end-position)))
            (when (and (get-text-property
                        bol 'agent-shell-markdown-list-rendered)
                       (not (get-text-property bol 'wrap-prefix)))
              (when-let* ((content (my/agent-shell--list-content-start bol eol))
                          (prefix (buffer-substring bol content)))
                (remove-text-properties 0 (length prefix)
                                        '(line-prefix nil wrap-prefix nil)
                                        prefix)
                (with-silent-modifications
                  (put-text-property
                   bol eol 'wrap-prefix
                   `(space :align-to
                           (,(string-pixel-width (concat base prefix)
                                                 (current-buffer)))))))))
          (forward-line 1)))))

  ;; Hooked from `my/agent-shell-setup-appearance' above, which runs once the
  ;; turn settles rather than mid-stream: while a response is still arriving the
  ;; list lines are rewritten repeatedly, and `shell-maker-finish-output-hook'
  ;; documents itself as the place to re-apply this sort of decoration.
  ;; Expanding a collapsed fragment renders it fresh, so that path is covered
  ;; too -- its hook runs narrowed to the body, which is why the sweep defaults
  ;; to the accessible portion rather than the whole buffer.

  ;; Workaround for an upstream shell-maker bug that wedges Emacs on
  ;; agent-shell startup.  Fragments that land above the prompt render
  ;; inside `agent-shell--with-buffer-narrowed-to' (agent-shell.el:5187),
  ;; and `shell-maker-with-auto-scroll-edit' runs within that narrowing.
  ;; `shell-maker--should-auto-scroll-p' then asks `pos-visible-in-window-p'
  ;; about the narrowed `point-max' while the window's layout still extends
  ;; past it: the display engine grinds for seconds and jit-lock signals
  ;; `args-out-of-range' at (1+ point-max), leaving Emacs in a redisplay
  ;; loop that only SIGUSR2 breaks.  shell-maker.el:1483-1490 documents the
  ;; hazard and swallows the signal, but only after paying for the layout;
  ;; decline the question instead, which is the fallback that comment
  ;; endorses.  Touches only above-prompt renders (startup, notices): while
  ;; a turn is in flight `:above-last-prompt' is nil and nothing narrows.
  ;; Remove once fixed upstream.
  (defun my/agent-shell-no-auto-scroll-when-narrowed (orig &rest args)
    "Skip the auto-scroll check while the buffer is narrowed.

ORIG is `shell-maker--should-auto-scroll-p' and ARGS its arguments."
    (if (buffer-narrowed-p)
        nil
      (apply orig args)))
  (advice-add 'shell-maker--should-auto-scroll-p :around
              #'my/agent-shell-no-auto-scroll-when-narrowed)
  :bind (:map agent-shell-mode-map
              ("RET" . newline)
              ("C-<return>" . shell-maker-submit)
              ("M-<return>" . shell-maker-submit)
              ("C-c C-c" . agent-shell-interrupt)))
