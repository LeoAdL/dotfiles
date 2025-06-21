;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-

(setopt mac-command-modifier 'meta
        mac-option-modifier 'none)

(defun my-after-frame (frame)
  (if (display-graphic-p frame)
      (progn
        (set-frame-font "Iosevka-18" nil t)
        (set-face-font 'default "Iosevka-18")
        (set-face-font 'fixed-pitch-serif "IBM Plex Serif")
        )))

(mapc 'my-after-frame (frame-list))
(add-hook 'after-make-frame-functions 'my-after-frame)

(setopt use-package-compute-statistics t)

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'elpaca-after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'elpaca-after-init-hook #'(lambda()
                                      (let ((inhibit-message t))
                                        (recentf-mode 1))))
(add-hook 'kill-emacs-hook #'recentf-cleanup)

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(add-hook 'elpaca-after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'elpaca-after-init-hook #'save-place-mode)

;; Idle garbage collection

(use-package general :ensure (:wait t)
  :config
  (general-auto-unbind-keys)
  (general-evil-setup)
  )

(defun +elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

;; You could embed this code directly in the reicpe, I just abstracted it into a function.
(defun +elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list '+elpaca-unload-seq 'elpaca--activate-package)))
(use-package seq :ensure `(seq :build ,(+elpaca-seq-build-steps)))

(use-package org
  :ensure t
  :commands (org-mode org-version)
  :mode
  ("\\.org\\'" . org-mode)
  :demand t
  :general (:prefix "SPC m"
                    :states 'normal
                    :keymaps 'override
                    "A" #'org-archive-subtree-default
                    "e" #'org-export-dispatch
                    "f" #'org-footnote-action
                    "h" #'org-toggle-heading
                    "i" #'org-toggle-item
                    "I" #'org-id-get-create
                    "k" #'org-babel-remove-result
                    "n" #'org-store-link
                    "o" #'org-set-property
                    "q" #'org-set-tags-command
                    "t" #'org-todo
                    "T" #'org-todo-list
                    "x" #'org-toggle-checkbox
                    "a a" #'org-attach
                    "a d" #'org-attach-delete-one
                    "a D" #'org-attach-delete-all
                    "a n" #'org-attach-new
                    "a o" #'org-attach-open
                    "a O" #'org-attach-open-in-emacs
                    "a r" #'org-attach-reveal
                    "a R" #'org-attach-reveal-in-emacs
                    "a u" #'org-attach-url
                    "a s" #'org-attach-set-directory
                    "a S" #'org-attach-sync
                    "b -" #'org-table-insert-hline
                    "b a" #'org-table-align
                    "b b" #'org-table-blank-field
                    "b c" #'org-table-create-or-convert-from-region
                    "b e" #'org-table-edit-field
                    "b f" #'org-table-edit-formulas
                    "b h" #'org-table-field-info
                    "b s" #'org-table-sort-lines
                    "b r" #'org-table-recalculate
                    "b R" #'org-table-recalculate-buffer-tables
                    "b d c" #'org-table-delete-column
                    "b d r" #'org-table-kill-row
                    "b i c" #'org-table-insert-column
                    "b i h" #'org-table-insert-hline
                    "b i r" #'org-table-insert-row
                    "b i H" #'org-table-hline-and-move
                    "b t f" #'org-table-toggle-formula-debugger
                    "b t o" #'org-table-toggle-coordinate-overlays
                    "c c" #'org-clock-cancel
                    "c d" #'org-clock-mark-default-task
                    "c e" #'org-clock-modify-effort-estimate
                    "c E" #'org-set-effort
                    "c g" #'org-clock-goto
                    "c i" #'org-clock-in
                    "c I" #'org-clock-in-last
                    "c o" #'org-clock-out
                    "c r" #'org-resolve-clocks
                    "c R" #'org-clock-report
                    "c t" #'org-evaluate-time-range
                    "c =" #'org-clock-timestamps-up
                    "c -" #'org-clock-timestamps-down
                    "d d" #'org-deadline
                    "d s" #'org-schedule
                    "d t" #'org-time-stamp
                    "d T" #'org-time-stamp-inactive
                                        ;"f o" #'consult-org-heading
                                        ;"f a" #'consult-org-agenda
                    "l c" #'org-cliplink
                    "l i" #'org-id-store-link
                    "l l" #'org-insert-link
                    "l L" #'org-insert-all-links
                    "l s" #'org-store-link
                    "l S" #'org-insert-last-stored-link
                    "l t" #'org-toggle-link-display
                    "p d" #'org-priority-down
                    "p p" #'org-priority
                    "p u" #'org-priority-up
                    )
  :config
  (setopt org-directory "~/org/")
  (setopt org-hide-emphasis-markers t)
  (setopt org-use-sub-superscripts '{})
  (setopt org-export-with-sub-superscripts t)
  (setopt org-preview-latex-image-directory "~/.cache/ltximg/")
  ;; ORG LATEX PREVIEW
  (setopt org-startup-with-latex-preview t)
  (setopt org-preview-latex-default-process 'dvisvgm)
  (setopt org-format-latex-options
          (plist-put org-format-latex-options :background "Transparent"))
  (setopt org-format-latex-options
          (plist-put org-format-latex-options :scale 2))
  (setopt
   org-agenda-files (list org-directory)                  ; Seems like the obvious place.
   org-log-done 'time                                     ; Having the time a item is done sounds convenient.
   org-list-allow-alphabetical t                          ; Have a. A. a) A) list bullets.
   )
  (setopt org-log-into-drawer t)
  (setopt org-log-state-notes-into-drawer t)
  (setopt org-babel-default-header-args
          '((:session . "none")
            (:results . "replace")
            (:exports . "code")
            (:cache . "no")
            (:noweb . "no")
            (:hlines . "no")
            (:tangle . "no")
            (:comments . "link")))


  (setopt org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-include-deadlines t
          org-agenda-block-separator nil
          org-agenda-tags-column 100 ;; from testing this seems to be a good value
          org-agenda-compact-blocks t
          org-agenda-time-grid
          '((daily today require-timed)
            (800 1000 1200 1400 1600 1800 2000 2200)
            " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
          org-agenda-current-time-string
          "◀── now ─────────────────────────────────────────────────")
  (setopt org-src-fontify-natively t
          org-auto-align-tags nil
          org-tags-column 0
          org-fontify-whole-heading-line t
          org-fontify-done-headline t
          org-insert-heading-respect-content t
          org-fontify-quote-and-verse-blocks t
          org-startup-with-inline-images t
          org-startup-indented t
          org-adapt-indentation nil
          org-edit-src-content-indentation 0
          org-startup-truncated nil
          org-fontify-done-headline t
          org-fontify-todo-headline t
          org-fontify-whole-heading-line t
          org-fontify-quote-and-verse-blocks t
          ;; Org styling, hide markup etc.
          org-pretty-entities t
          org-hide-leading-stars t
          org-priority-highest ?A
          org-priority-lowest ?E
          org-todo-keywords '((sequence "TODO(t)" "DOING" "DONE"))
          org-todo-keywords-for-agenda '((sequence "TODO" "DOING" "DONE")))
  (setopt org-highlight-latex-and-related '(native script entities))
  (setopt org-capture-templates
          '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
             "* TODO [#B] %?\n:Created: %T\n")
            ("j" "Journal" entry (file+olp+datetree "~/org/journal.org")
             "* %?\nEntered on %U\n  %i\n  %a")
            ("P" "process-soon" entry (file+headline "todo.org" "Todo")
             "* TODO %:fromname: %a %?\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")
            ("c" "Contact" entry (file "~/org/contacts.org")
             "* %?
 :PROPERTIES:
 :ADDRESS:
 :BIRTHDAY:
 :EMAIL:
 :NOTE:
 :END:"
             :empty-lines 1)
            ("w" "Work")
            ("wp" "Phone Call" entry (file+olp+datetree "~/org/work.org") "* Phone call about %?\nSCHEDULED:%t\nDEADLINE: %^T\n\n%i" :clock-in t)
            ("wm" "Meeting"    entry (file+olp+datetree "~/org/work.org") "* Meeting about %?\nSCHEDULED:%t\nDEADLINE: %^T\n\n%i"    :clock-in t)
            ("m" "Email Workflow")
            ("mw" "Write" entry (file+olp "~/org/mail.org" "New")
             "* TODO Email %?\nSCHEDULED:%t\nDEADLINE: %^T\n\n%i" :immediate-finish t)
            ("mf" "Follow Up" entry (file+olp "~/org/mail.org" "Follow Up")
             "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
            ("mr" "Read Later" entry (file+olp "~/org/mail.org" "Read Later")
             "* TODO Read %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t)
            ))
  )

(use-package vterm
  :ensure nil
  :defer t
  :commands vterm
  :config
  ;; Speed up vterm
  (setopt vterm-kill-buffer-on-exit t)

  (setopt vterm-timer-delay 0.01)
  ;; 5000 lines of scrollback, instead of 1000
  (setopt vterm-max-scrollback 5000))

(use-package multi-vterm
  :ensure t
  :after vterm
  :config
  (setopt vterm-keymap-exceptions nil)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev))

;; Tip: You can remove the `vertico-mode' use-package and replace it
;;      with the built-in `fido-vertical-mode'.
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :hook (elpaca-after-init . vertico-mode)
  :config
  (setopt vertico-cycle t)
  )

(use-package nerd-icons-completion
  :after marginalia
  :defer t
  :ensure t
  :init
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode)
  )

(use-package orderless
  ;; Vertico leverages Orderless' flexible matching capabilities, allowing users
  ;; to input multiple patterns separated by spaces, which Orderless then
  ;; matches in any order against the candidates.
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  ;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
  ;; In addition to that, Marginalia also enhances Vertico by adding rich
  ;; annotations to the completion candidates displayed in Vertico's interface.
  :ensure t
  :defer t
  :commands (marginalia-mode marginalia-cycle)
  :hook (elpaca-after-init . marginalia-mode))

(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :init
  (setopt prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setopt prefix-help-command #'embark-prefix-help-command)
  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package embark-consult
  :ensure t
  :defer t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :ensure t
  :general (
            :prefix "SPC"
            :keymaps 'override
            :states 'normal
            "/" #'consult-ripgrep
            "f f" #'consult-fd
            "f x" #'consult-mode-command
            "f h" #'consult-history
            "f k" #'consult-kmacro
            "f m" #'consult-man
            "f i" #'consult-info
            "f r" #'consult-recent-file
            ;; C-x bindings in `ctl-x-map'
            "f M-:" #'consult-complex-command
            "b b" #'consult-buffer
            "4 b" #'consult-buffer-other-window
            "5 b" #'consult-buffer-other-frame
            "t b" #'consult-buffer-other-tab
            "r b" #'consult-bookmark
            "p b" #'consult-project-buffer
            ;; Custom M-# bindings for fast register access
            "r r" #'consult-register
            ;; Other custom bindings
            "y" #'consult-yank-pop
            ;; g bindings in `goto-map'
            "G e" #'consult-compile-error
            "c b" #'consult-flymake
            "G g" #'consult-goto-line
            "G o" #'consult-outline
            "G m" #'consult-mark
            "G k" #'consult-global-mark
            "G i" #'consult-imenu
            "G I" #'consult-imenu-multi
            ;; M-s bindings in `search-map'
            "s d" #'consult-find
            "s c" #'consult-locate
            "s g" #'consult-grep
            "s G" #'consult-git-grep
            "s l" #'consult-line
            "s L" #'consult-line-multi
            "s k" #'consult-keep-lines
            "s u" #'consult-focus-lines
            ;; Isearch integration
            "s e" #'consult-isearch-history
            ;; Minibuffer history
            "s s" #'consult-history)


  ;; Enable automatic preview at point in the *Completions* buffer.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Tweak the register preview for `consult-register-load',
  ;; `consult-register-store' and the built-in commands.  This improves the
  ;; register formatting, adds thin separator lines, register sorting and hides
  ;; the window mode line.
  (setopt register-preview-delay 0.5
          register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))
  (setopt consult-narrow-key "<")
  (setopt consult-async-min-input 2
          consult-async-refresh-delay  0.15
          consult-async-input-throttle 0.2
          consult-async-input-debounce 0.1)

  )

(use-package consult-dir
  :ensure t
  :defer t
  :general (:prefix "SPC"
                    :keymaps 'override
                    :states 'normal
                    "f d" #'consult-dir)
  )

(eval-when-compile
  ;; It has to be defined before evil
  (setopt evil-want-integration t)
  (setopt evil-want-keybinding nil))

(setopt evil-undo-system 'undo-fu)

(use-package evil
  :ensure t
  :defer t
  :commands (evil-mode evil-define-key)
  :hook (elpaca-after-init . evil-mode)
  :commands (evil-mode evil-define-key)
  :init
  (setopt evil-want-C-u-scroll t)
  (setopt evil-want-fine-undo t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (setopt evil-ex-search-vim-style-regexp t
          evil-ex-visual-char-range t  ; column range for ex commands
          evil-mode-line-format 'nil
          ;; more vim-like behavior
          evil-symbol-word-search t
          ;; if the current state is obvious from the cursor's color/shape, then
          ;; we won't need superfluous indicators to do it instead.
          evil-normal-state-cursor 'box
          evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
          evil-insert-state-cursor 'bar
          evil-visual-state-cursor 'hollow
          ;; Only do highlighting in selected window so that Emacs has less work
          ;; to do highlighting them all.
          evil-ex-interactive-search-highlight 'selected-window
          ;; It's infuriating that innocuous "beginning of line" or "end of line"
          ;; errors will abort macros, so suppress them:
          evil-kbd-macro-suppress-motion-error t
          ))

(use-package evil-collection
  :ensure t
  :after evil 
  :config
  (evil-collection-init)
  )

(use-package evil-args
  :ensure t
  :after evil)

(use-package evil-goggles
  :ensure t
  :after evil
  :defer t
  :hook (evil-mode . evil-goggles-mode)
  :config
  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-anzu
  :ensure (evil-anzu :type git :host github :repo "emacsorphanage/evil-anzu")
  :hook (elpaca-after-init . global-anzu-mode)
  :init
  (with-eval-after-load 'evil
    (require 'evil-anzu))
  )

(use-package vimish-fold
  :ensure t
  :defer t
  :after evil )

(use-package evil-vimish-fold
  :ensure t
  :after (vimish-fold)
  :hook ((prog-mode conf-mode text-mode) . evil-vimish-fold-mode))

(use-package ibuffer-vc
  :ensure t
  :after ibuffer
  )

(use-package nerd-icons-ibuffer
  :ensure t
  :defer t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package undo-fu
  :ensure t
  :defer t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 256000           ; 256kb (default is 160kb)
        undo-strong-limit 2000000   ; 2mb   (default is 240kb)
        undo-outer-limit 36000000)  ; 36mb  (default is 24mb)
  )

(use-package undo-fu-session
  :ensure t
  :defer t
  :commands undo-fu-session-global-mode
  :hook (elpaca-after-init . undo-fu-session-global-mode)
  :config
  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst))
  )

(use-package vundo
  :ensure t
  :defer t
  :general (
            :prefix "SPC"
            :keymaps 'override
            :states 'normal
            "s u" #'vundo)
  :config
  (setopt vundo-glyph-alist vundo-unicode-symbols
          vundo-compact-display t)
  )

(use-package evil-visualstar
  :after evil
  :ensure t
  :defer t
  :hook (evil-mode . global-evil-visualstar-mode)
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward))

(use-package evil-surround
  :after evil
  :ensure t
  :defer t
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :hook (evil-mode . global-evil-surround-mode))

(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))

(use-package evil-snipe
  :defer t
  :after evil
  :hook (evil-mode . evil-snipe-mode)
  :commands (evil-snipe-mode))

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)
  :hook (elpaca-after-init . global-corfu-mode)
  ;; Enable Corfu
  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :config
  (setopt corfu-auto-prefix 2)
  (setopt corfu-auto-delay 0.1)
  (setopt corfu-quit-no-match t)
  (setopt corfu-auto t)
  (setopt corfu-preselect 'prompt)
  )

(use-package org-block-capf
  :ensure (org-block-capf  :type git :host github :repo "xenodium/org-block-capf")
  :after org
  :hook (org-mode . org-block-capf-add-to-completion-at-point-functions))

(use-package cape-keyword
  :ensure (cape-keyword :type git :host github :repo "minad/cape")
  :defer t
  :config
  (add-to-list 'cape-keyword-list '(q-mode
                                    "abs" "cor" "ej" "gtime" "like" "mins" "prev" "scov" "system" "wavg" "acos" "cos" "ema" "hclose" "lj" "ljf" "mmax" "prior" "sdev" "tables" "where" "aj" "aj0" "count" "enlist" "hcount" "load" "mmin" "rand" "select" "tan" "while" "ajf" "ajf0" "cov" "eval" "hdel" "log" "mmu" "rank" "set" "til" "within" "all" "cross" "except" "hopen" "lower" "mod" "ratios" "setenv" "trim" "wj" "wj1" "and" "csv" "exec" "hsym" "lsq" "msum" "raze" "show" "type" "wsum" "any" "cut" "exit" "iasc" "ltime" "neg" "read0" "signum" "uj" "ujf" "xasc" "asc" "delete" "exp" "idesc" "ltrim" "next" "read1" "sin" "ungroup" "xbar" "asin" "deltas" "fby" "if" "mavg" "not" "reciprocal" "sqrt" "union" "xcol" "asof" "desc" "fills" "ij" "ijf" "max" "null" "reval" "ss" "update" "xcols" "atan" "dev" "first" "in" "maxs" "or" "reverse" "ssr" "upper" "xdesc" "attr" "differ" "fkeys" "insert" "mcount" "over" "rload" "string" "upsert" "xexp" "avg" "distinct" "flip" "inter" "md5" "parse" "rotate" "sublist" "value" "xgroup" "avgs" "div" "floor" "inv" "mdev" "peach" "rsave" "sum" "var" "xkey" "bin" "binr" "do" "get" "key" "med" "pj" "rtrim" "sums" "view" "xlog" "ceiling" "dsave" "getenv" "keys" "meta" "prd" "save" "sv" "views" "xprev" "cols" "each" "group" "last" "min" "prds" "scan" "svar" "vs" "xrank" ".Q.ajf0" ".Q.sx" ".Q.k" ".Q.K" ".Q.host" ".Q.addr" ".Q.gc" ".Q.ts" ".Q.gz" ".Q.w" ".Q.res" ".Q.addmonths" ".Q.f" ".Q.fmt" ".Q.ff" ".Q.fl" ".Q.opt" ".Q.def" ".Q.ld" ".Q.qt" ".Q.v" ".Q.qp" ".Q.V" ".Q.ft" ".Q.ord" ".Q.nv" ".Q.tx" ".Q.tt" ".Q.fk" ".Q.t" ".Q.ty" ".Q.nct" ".Q.fu" ".Q.fc" ".Q.A" ".Q.a" ".Q.n" ".Q.nA" ".Q.an" ".Q.b6" ".Q.Aa" ".Q.unm" ".Q.id" ".Q.j10" ".Q.x10" ".Q.j12" ".Q.x12" ".Q.btoa" ".Q.sha1" ".Q.prf0" ".Q.objp" ".Q.lo" ".Q.l" ".Q.sw" ".Q.tab" ".Q.t0" ".Q.s1" ".Q.s2" ".Q.S" ".Q.s" ".Q.hap" ".Q.hmb" ".Q.hg" ".Q.hp" ".Q.a1" ".Q.a0" ".Q.IN" ".Q.qa" ".Q.qb" ".Q.vt" ".Q.bvfp" ".Q.bvi" ".Q.bv" ".Q.sp" ".Q.pm" ".Q.pt" ".Q.MAP" ".Q.dd" ".Q.d0" ".Q.p1" ".Q.p2" ".Q.p" ".Q.view" ".Q.jp" ".Q.rp" ".Q.fobj" ".Q.L1" ".Q.L" ".Q.li" ".Q.cn" ".Q.pcnt" ".Q.dt" ".Q.ind" ".Q.fp" ".Q.foo" ".Q.a2" ".Q.qd" ".Q.xy" ".Q.x1" ".Q.x0" ".Q.x2" ".Q.ua" ".Q.q0" ".Q.qe" ".Q.ps" ".Q.enxs" ".Q.enx" ".Q.en" ".Q.ens" ".Q.par" ".Q.dpts" ".Q.dpt" ".Q.dpfts" ".Q.dpft" ".Q.hdpf" ".Q.fsn" ".Q.fs" ".Q.fpn" ".Q.fps" ".Q.dsftg" ".Q.M" ".Q.chk" ".Q.Ll" ".Q.Lp" ".Q.Lx" ".Q.Lu" ".Q.Ls" ".Q.fqk" ".Q.fql" ".Q.btx" ".Q.bt" ".Q.sbt" ".Q.trp" ".Q.trpd" ".Q.dr" ".Q.dw" ".Q.pl0" ".Q.pl" ".Q.jl8" ".Q.srr" ".Q.prr" ".Q.lu" ".Q.DL" ".Q.dbg" ".Q.err" ".Q.BP" ".Q.bp" ".Q.bs" ".Q.bu" ".Q.bd" ".Q.bc" ".h.htc" ".h.hta" ".h.htac" ".h.ha" ".h.hb" ".h.pre" ".h.xmp" ".h.d" ".h.cd" ".h.td" ".h.hc" ".h.xs" ".h.xd" ".h.ex" ".h.iso8601" ".h.eb" ".h.es" ".h.ed" ".h.edsn" ".h.ec" ".h.tx" ".h.xt" ".h.ka" ".h.c0" ".h.c1" ".h.logo" ".h.sa" ".h.html" ".h.sb" ".h.fram" ".h.jx" ".h.uh" ".h.sc" ".h.hug" ".h.hu" ".h.ty" ".h.hnz" ".h.hn" ".h.HOME" ".h.hy" ".h.hp" ".h.he" ".h.val" ".h.br" ".h.hr" ".h.nbr" ".h.code" ".h.http" ".h.text" ".h.data" ".h.ht" ".j.e" ".j.q" ".j.s" ".j.es" ".j.J" ".j.k" ".j.jd" ".j.j"
                                    ))
  )

(use-package cape
  :ensure t
  :defer t
  :commands (cape-history cape-dabbrev cape-file cape-keyword)
  :bind ("C-c p" . cape-prefix-map)
  :config
  (setopt cape-dabbrev-min-length 1)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  )

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :init
  ;; Optionally:
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (setopt nerd-icons-corfu-mapping
          '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
            (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
            ;; ...
            (t :style "cod" :icon "code" :face font-lock-warning-face)))
  ;; Remember to add an entry for `t', the library uses that as default.
  )

(use-package corfu-history
  :ensure (corfu-history :type git :host github :repo "minad/corfu")
  :hook (corfu-mode . corfu-history-mode)
  :defer t
  :init
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :ensure (corfu-popupinfo :type git :host github :repo "minad/corfu")
  :hook (corfu-mode . corfu-popupinfo-mode)
  :defer t
  :config
  (setopt corfu-popupinfo-delay '(0.5 . 1.0)))

;; Hide warnings and display only errors
(setopt warning-minimum-level :error)

;; Display of line numbers in the buffer:
;; (display-line-numbers-mode 1)

(use-package which-key
  :ensure t ; builtin
  :defer t
  :commands which-key-mode
  :hook (elpaca-after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40))

(unless (and (eq window-system 'mac)
             (bound-and-true-p mac-carbon-version-string))
  ;; Enables `pixel-scroll-precision-mode' on all operating systems and Emacs
  ;; versions, except for emacs-mac.
  ;;
  ;; Enabling `pixel-scroll-precision-mode' is unnecessary with emacs-mac, as
  ;; this version of Emacs natively supports smooth scrolling.
  ;; https://bitbucket.org/mituharu/emacs-mac/commits/65c6c96f27afa446df6f9d8eff63f9cc012cc738
  (setopt pixel-scroll-precision-use-momentum nil) ; Precise/smoother scrolling
  (pixel-scroll-precision-mode 1))

;; Display the time in the modeline
;; Display the time in the modeline
(setopt display-time-mail-string "")
(add-hook 'elpaca-after-init-hook #'display-time-mode)

;; Paren match highlighting
(add-hook 'elpaca-after-init-hook #'show-paren-mode)

;; Track changes in the window configuration, allowing undoing actions such as
;; closing windows.
(add-hook 'elpaca-after-init-hook #'winner-mode)
(add-hook 'elpaca-after-init-hook #'auto-save-visited-mode)
(add-hook 'elpaca-after-init-hook #'global-visual-line-mode)
(add-hook 'elpaca-after-init-hook #'delete-selection-mode)

;; Configure Emacs to ask for confirmation before exiting
(setopt confirm-kill-emacs 'y-or-n-p)

(setopt make-backup-files nil)
(setopt vc-make-backup-files nil)
(setopt kept-old-versions 10)
(setopt kept-new-versions 10)
(setopt redisplay-skip-fontification-on-input t)
;; Window dividers separate windows visually. Window dividers are bars that can
;; be dragged with the mouse, thus allowing you to easily resize adjacent
;; windows.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Window-Dividers.html
(add-hook 'elpaca-after-init-hook #'window-divider-mode)

;; Automatically hide file details (permissions, size, modification date, etc.)
;; in Dired buffers for a cleaner display.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Configure Emacs to ask for confirmation before exiting
(setopt confirm-kill-emacs 'y-or-n-p)

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator "•")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))


(use-package doom-modeline
  :ensure t
  :defer t
  :hook ((elpaca-after-init . doom-modeline-mode)
         (doom-modeline-mode . size-indication-mode) ; filesize in modeline
         (doom-modeline-mode . column-number-mode))   ; cursor column in modeline
  :config
  (setopt doom-modeline-hud t)
  (setopt doom-modeline-buffer-encoding nil)
  (setopt doom-modeline-unicode-fallback t)
  (setopt doom-modeline-time-analogue-clock nil)
  (setopt find-file-visit-truename t)
  ;; (setopt nerd-icons-scale-factor 1)
  ;; (setopt doom-modeline-height 1) ; optional
  (setopt doom-modeline-project-detection 'project)
  (setopt mode-line-right-align-edge 'right-fringe)
  )

(defun er-disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))
(defun er-load-theme (theme)
  (er-disable-all-active-themes)
  (load-theme theme t))

(defun er-new-load-theme ()
  (interactive)
  (er-disable-all-active-themes)
  (call-interactively 'load-theme))

(use-package doom-themes
  :ensure t
  :config
  (setopt doom-themes-enable-bold t)
  (setopt doom-themes-enable-italic t)
  (setopt doom-themes-padded-modeline t)
  (doom-themes-visual-bell-config)
  )

(use-package solaire-mode
  :ensure t
  :defer t
  :hook (elpaca-after-init . solaire-global-mode)
  )

(defun er-disable-all-active-themes ()
  "Disable all currently active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(use-package catppuccin-theme
  :ensure t
  :defer t
  :init
  (setopt catppuccin-enlarge-headings nil)
  ;; Adjust font size of titles level 1 (default 1.3)
  (setopt catppuccin-height-title-1 1.25)
  ;; Adjust font size of titles level 2 (default 1.1)
  (setopt catppuccin-height-title-2 1.15)
  ;; Adjust font size of titles level 3 (default 1.0)
  (setopt catppuccin-height-title-3 1.05)
  ;; Adjust font size of document titles (default 1.44)
  (setopt catppuccin-height-doc-title 1.4)
  ;; Use background color to make highlighted matches more visible. (default nil)
  (setopt catppuccin-highlight-matches t)
  ;; Use :slant italic for comments. (default nil)
  (setopt catppuccin-italic-comments t)
  ;; Use :slant italic for blockquotes in markdown and org. (default nil)
  (setopt catppuccin-italic-blockquotes t)
  (setopt catppuccin-highlight-matches t)
  (setopt catppuccin-flavor 'mocha)
  (er-disable-all-active-themes)
  (load-theme 'catppuccin :no-confirm))

(use-package diredfl
  :ensure t
  :defer t
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(use-package tramp
  :ensure nil
  :defer t
  :config
  ;; Enable full-featured Dirvish over TRAMP on ssh connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))
  (connection-local-set-profiles
   '(:application tramp :protocol "ssh")
   'remote-direct-async-process)
  ;; Tips to speed up connections
  (setopt tramp-verbose 0)
  (setopt tramp-chunksize 2000)
  (setopt tramp-ssh-controlmaster-options nil))

(use-package dirvish
  :ensure t
  :after evil
  :defer t
  :init
  (dirvish-override-dired-mode)
                                        ; :hook (dired-mode . diff-hl-dired-mode)
  :general
  (:states 'normal
           :keymaps 'dirvish-mode-map
           "a"   #'dirvish-quick-access
           "q"   #'dirvish-quit
           "W"   #'wdired-change-to-wdired-mode
           "F"   #'dirvish-file-info-menu
           "p"   #'dirvish-rsync
           "/"   #'dirvish-narrow
           "u"   #'dired-undo
           "U"   #'dired-unmark
           "C-n"   #'dired-next-line
           "C-p"   #'dired-previous-line
           "f"   #'dirvish-fd-ask
           "^"   #'dirvish-history-last
           "h"   #'dirvish-history-jump
           "s"   #'dirvish-quicksort
           "v"   #'dirvish-vc-menu
           "TAB" #'dirvish-subtree-toggle
           "M-f" #'dirvish-history-go-forward
           "M-b" #'dirvish-history-go-backward
           "M-l" #'dirvish-ls-switches-menu
           "M-m" #'dirvish-mark-menu
           "M-t" #'dirvish-layout-toggle
           "M-s" #'dirvish-setup-menu
           "M-e" #'dirvish-emerge-menu
           "M-j" #'dirvish-fd-jump)

  :config
  (evil-make-overriding-map dirvish-mode-map 'normal)
  (when (string= system-type "darwin")
    (setopt insert-directory-program "gls")
    (setopt dired-listing-switches "-aBhl --group-directories-first")
    )
  (setopt dirvish-attributes'(vc-state subtree-state nerd-icons git-msg file-time file-size))
  (setopt dirvish-default-layout '(0 0.4 0.6))
  (setopt dirvish-rsync-program "/run/current-system/sw/bin/rsync")
  (setopt dirvish-yank-rsync-args '("-s" "--archive" "--verbose" "--compress" "--info=progress2" "--partial"))
  (general-define-key
   :prefix "SPC"
   :keymaps 'override
   :states 'normal
   "." #'find-file)
  )


(use-package hl-todo
  :ensure t
  :defer t
  :hook (prog-mode . hl-todo-mode)
  :config
  (setopt hl-todo-highlight-punctuation ":"
          hl-todo-keyword-faces
          '(;; For reminders to change or add something at a later date.
            ("TODO" warning bold)
            ;; For code (or code paths) that are broken, unimplemented, or slow,
            ;; and may become bigger problems later.
            ("FIXME" error bold)
            ;; For code that needs to be revisited later, either to upstream it,
            ;; improve it, or address non-critical issues.
            ("REVIEW" font-lock-keyword-face bold)
            ;; For code smells where questionable practices are used
            ;; intentionally, and/or is likely to break in a future update.
            ("HACK" font-lock-constant-face bold)
            ;; For sections of code that just gotta go, and will be gone soon.
            ;; Specifically, this means the code is deprecated, not necessarily
            ;; the feature it enables.
            ("DEPRECATED" font-lock-doc-face bold)
            ;; Extra keywords commonly found in the wild, whose meaning may vary
            ;; from project to project.
            ("NOTE" success bold)
            ("BUG" error bold)
            ("XXX" font-lock-constant-face bold))))

(use-package indent-bars
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook #'indent-bars-mode)
  :custom
  (indent-bars-prefer-character t)
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  ;; Add other languages as needed
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	                                   if_statement with_statement while_statement)))
  ;; wrap may not be needed if no-descend-list is enough
  ;;(indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;				      list list_comprehension
  ;;				      dictionary dictionary_comprehension
  ;;				      parenthesized_expression subscript)))
  :config
  (setopt
   indent-bars-starting-column 0
   indent-bars-color '(highlight :face-bg t :blend 0.05)
   indent-bars-no-stipple nil
   indent-bars-pattern "."
   indent-bars-width-frac 0.15
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 0.95) ; blend=1: blend with BG only
   indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
   indent-bars-display-on-blank-lines t))

(use-package ligature
  :ensure t
  :defer t
  :hook (elpaca-after-init . global-ligature-mode)
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  )

                                        ; (use-package diff-hl
                                        ;   :ensure t
                                        ;   :defer t
                                        ;   :hook
                                        ;   ((elpaca-after-init . global-diff-hl-mode)
                                        ;    (magit-post-refresh . diff-hl-magit-post-refresh)
                                        ;   )
                                        ; :config
                                        ; (setopt diff-hl-global-modes '(not image-mode pdf-view-mode))
                                        ; ;; PERF: A slightly faster algorithm for diffing.
                                        ;                                         ; (setopt vc-git-diff-switches '("--histogram"))
                                        ; ;; PERF: Slightly more conservative delay before updating the diff
                                        ;                                         ; (setopt diff-hl-flydiff-delay 0.5)  ; default: 0.3
                                        ; ;; PERF: don't block Emacs when updating vc gutter
                                        ; (setopt diff-hl-update-async t)
                                        ; ;; UX: get realtime feedback in diffs after staging/unstaging hunks.
                                        ; (setopt diff-hl-show-staged-changes nil)
                                        ; )

(use-package transient
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t
  :after transient
  :config
  (setopt magit-format-file-function #'magit-format-file-nerd-icons)
  (setopt transient-default-level 5
          magit-diff-refine-hunk t ; show granular diffs in selected hunk
          ;; Don't autosave repo buffers. This is too magical, and saving can
          ;; trigger a bunch of unwanted side-effects, like save hooks and
          ;; formatters. Trust the user to know what they're doing.
          magit-save-repository-buffers nil
          ;; Don't display parent/related refs in commit buffers; they are rarely
          ;; helpful and only add to runtime costs.
          magit-revision-insert-related-refs nil)
  )

(use-package magit-todos
  :ensure t
  :after magit
  :hook (magit-mode . magit-todos-mode))

(use-package git-timemachine
  :defer t
  :ensure (git-timemachine :type git :host codeberg :repo "pidu/git-timemachine")
  :config
  (setopt git-timemachine-show-minibuffer-details t)
  )

(use-package flymake-popon
  :defer t
  :hook (flymake-mode . flymake-popon-mode)
  :ensure t)

;; (use-package flymake-vale
;;   :ensure (flymake-vale :type git :host github :repo "tpeacock19/flymake-vale")
;;   :defer t
;;   :after flymake
;;   :init
;;   (setopt flymake-vale-program-args "--config=$HOME/.config/vale/.vale.ini")
;;   (add-hook 'text-mode-hook #'flymake-vale-load)
;;   (add-hook 'mu4e-compose-mode-hook #'flymake-vale-load)
;;   (add-hook 'latex-mode-hook #'flymake-vale-load)
;;   (add-hook 'org-mode-hook #'flymake-vale-load)
;;   (add-hook 'markdown-mode-hook #'flymake-vale-load)
;;   (add-hook 'message-mode-hook #'flymake-vale-load)
;;   )

(use-package lsp-mode
  :ensure t
  :general
  (:states 'normal
           :desc "Jump to definition"                    "g d"   #'xref-find-definitions
           :desc "Jump to references"                    "g r"   #'xref-find-references
           :desc "Jump to implementations"                    "g i"   #'lsp-find-implementation
           :desc "Jump to declarations"                    "g D"   #'lsp-find-declarations)
  ([remap xref-find-apropos] #'lsp-describe-thing-at-point)
  :defer t
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :config
  (setopt lsp-enable-suggest-server-download t)
  (setopt lsp-warn-no-matched-clients nil))

(use-package consult-lsp
  :ensure t
  :defer t)

(use-package lsp-pyright
  :ensure t
  :defer t
  :custom (lsp-pyright-langserver-command "basedpyright") ;; or basedpyright
  :hook (python-ts-mode . (lambda ()
                            (require 'lsp-pyright)
                            (lsp-deferred))))  ; or lsp-deferred
(use-package yasnippet
  :ensure t
  :defer t
  :hook (elpaca-after-init . yas-global-mode)
  :config
  (setopt yas-triggers-in-field t))

(use-package auto-yasnippet
  :ensure t
  :defer t)

(use-package doom-snippets
  :after yasnippet
  :ensure (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*")))
;; (use-package org-contrib
;;   :after org
;;   :ensure t)

(use-package ox-clip
  :after ox
  :ensure t)

(use-package org-cliplink
  :after org
  :ensure t)

(use-package toc-org
  :after org
  :ensure t)


(use-package evil-org
  :after org
  :defer t
  :ensure (evil-org :type git :host github :repo "doomelpa/evil-org-mode")
  :hook (org-mode . evil-org-mode)
  :config
  (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  )

(use-package evil-org-agenda
  :after org-agenda
  :defer t
  :ensure nil
  :hook (org-agenda-mode . evil-org-agenda-mode)
  :config
  (evil-org-agenda-set-keys)
  )

(use-package orgit
  :after (org magit)
  :ensure t)

(use-package jupyter
  :ensure t
  :defer t
  :init
  (setopt jupyter-use-zmq t
          jupyter-repl-completion-at-point-hook-depth 2
          jupyter-eval-use-overlays nil
          jupyter-eval-short-result-max-lines 0
          jupyter-eval-overlay-keymap "<backtab>"
          jupyter-default-notebook-port 8895)
  :bind (("<backtab>" . jupyter-eval-toggle-overlay)))

(use-package code-cells
  :defer t
  :ensure t)

(use-package evil-textobj-tree-sitter
  :after (evil treesit)
  :ensure t)

(when (string= system-type "darwin")
  (use-package mu4e
    :ensure nil
    :demand t
    :after org
    :commands mu4e mu4e-compose-new
    :config
    (setopt mail-user-agent 'mu4e-user-agent
            message-mail-user-agent 'mu4e-user-agent)
    (setopt sendmail-program (executable-find "msmtp")
            send-mail-function #'smtpmail-send-it
            message-sendmail-f-is-evil t
            message-sendmail-extra-arguments '("--read-envelope-from")
            message-send-mail-function #'message-send-mail-with-sendmail
            mml-secure-openpgp-signers '("6A5C039B63B86AC6C5109955B57BA04FBD759C7F" "D1D9947126EE64AC7ED3950196F352393B5B3C2E")
            mml-secure-openpgp-sign-with-sender t
            mu4e-use-fancy-chars t                   ; allow fancy icons for mail threads
            mu4e-notification-support t
            mu4e-change-filenames-when-moving t
            mu4e-index-lazy-check nil
            mu4e-search-results-limit 100
            mu4e-context-policy 'pick-first ;; Always ask which context to use when composing a new mail
            mu4e-compose-context-policy 'ask ;; Always ask which context to use when composing a new mail
            mu4e-update-interval 60
            mu4e-get-mail-command "mbsync -a"
            mu4e-mu-allow-temp-file t
            message-kill-buffer-on-exit t
            mu4e-compose-complete-only-after "2015-01-01"
            mu4e-headers-date-format "%d/%m/%y"
            mu4e-headers-time-format "⧖ %H:%M"
            message-dont-reply-to-names #'mu4e-personal-or-alternative-address-p
            mu4e-bookmarks '((:name "Unread messages" :query "flag:unread AND maildir:/.*inbox/" :key 117)
                             (:name "Today's messages" :query "date:today..now AND maildir:/.*inbox/" :key 116)
                             (:name "Flagged messages" :query "flag:flagged" :key 102)
                             (:name "Unified inbox" :query "maildir:/.*inbox/" :key 105)
                             (:name "Sent" :query "maildir:/.*Sent/" :key 115)
                             (:name "Drafts" :query "maildir:/.*Drafts/" :key 100)
                             (:name "Spam" :query "maildir:/.*Spam/ or maildir:/.*Junk/" :key 83)
                             (:name "Trash" :query "maildir:/.*Trash/" :key 84))
            mu4e-read-option-use-builtin t
            mu4e-attachment-dir "~/Downloads"
            mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
            mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶ ")
            mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
            mu4e-headers-thread-first-child-prefix   '("├>" . "├▶")
            mu4e-headers-thread-child-prefix         '("├>" . "├▶")
            mu4e-headers-thread-last-child-prefix    '("└>" . "╰▶")
            mu4e-contexts
            `( ,(make-mu4e-context
                 :name "Personal"
                 :enter-func (lambda () (mu4e-message "Entering Personal context"))
                 :leave-func (lambda () (mu4e-message "Leaving Personal context"))
                 ;; we match based on the contact-fields of the message
                 :match-func (lambda (msg)
                               (when msg
                                 (mu4e-message-contact-field-matches msg
                                                                     :to "leoaparisi@gmail.com")))
                 :vars '( ( user-mail-address	    . "leoaparisi@gmail.com"  )
                          (mu4e-sent-folder       . "/[Gmail]/Sent Mail")
                          (mu4e-drafts-folder     . "/[Gmail]/Drafts")
                          (mu4e-trash-folder      . "/[Gmail]/Trash")
                          (mu4e-refile-folder     . "/Archives")
                          (user-mail-address . "leoaparisi@gmail.com")
                          (smtpmail-smtp-user     . "leoaparisi@gmail.com")
                          ( user-full-name	    . "Leo Aparisi de Lannoy" )))))
    ;; (setopt mu4e-thread-mode t)
    (add-hook 'completion-at-point-functions #'mu4e-complete-contact)
    (setopt gnus-icalendar-org-capture-file "~/org/notes.org"
            gnus-icalendar-org-capture-headline '("Calendar"))
    )

  (use-package org-mime
    :ensure t
    :defer t
    :after (mu4e org)
    :config
    (setopt org-mime-export-options '(:with-latex mathjax))
    )
  )

(add-hook 'conf-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'text-mode-hook #'flymake-mode)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(general-define-key
 :keymaps '(normal insert emacs)
 "C-=" #'global-text-scale-adjust)


(use-package org-modern
  :ensure t
  :defer t
  :after org
  :hook ((org-agenda-finalize . org-modern-agenda)
         (org-mode . global-org-modern-mode))
  :config
  (setopt org-modern-star 'replace
          org-modern-hide-stars nil
          org-modern-table-vertical 1
          org-modern-table-horizontal 0.2
          org-modern-block-name t
          org-modern-horizontal-rule t
          org-modern-todo-faces
          '(("TODO" :inverse-video t :foreground "indian red")
            ("DOING"  :inverse-video t :foreground "medium aquamarine")
            ("DONE"  :inverse-video t :foreground "slate gray"))
          org-modern-priority-faces
          '((?A :background "indian red" :foreground "black")
            (?B :background "light salmon" :foreground "black")
            (?C :background "rosy brown" :foreground "black")
            (?D :background "NavajoWhite" :foreground "black")
            (?E  :background "bisque" :foreground "black"))
          org-modern-keyword t)
  )

(use-package org-appear
  :ensure t
  :defer t
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setopt org-appear-autoemphasis t
          org-appear-autosubmarkers t
          org-appear-autolinks t
          org-appear-autokeywords t
          org-appear-autoentities t
          org-appear-inside-latex nil
          org-appear-autosubmarkers t))

(use-package org-fragtog
  :ensure t
  :after org
  :defer t
  :hook (org-mode . org-fragtog-mode))

(use-package org-pandoc-import
  :ensure (org-pandoc-import :type git :host github
                             :repo "tecosaur/org-pandoc-import"
                             :files ("*.el" "filters" "preprocessors"))
  :after ox
  :config
  (org-pandoc-import-backend jira))

(use-package q-mode
  :defer t
  :ensure t
  :config (setopt
           q-program "q -s 7"))

(use-package vlf
  :ensure t
  :defer t
  :init
  (require 'vlf-setup)
  :config
  (setopt vlf-application "dont-ask")
  )

(use-package csv-mode
  :defer t
  :ensure (csv-mode :type git :host github :repo "emacsmirror/csv-mode":branch "master" )
  :hook ((csv-mode . csv-align-mode)
         (csv-mode . csv-header-line))
  )

(use-package rainbow-csv
  :defer t
  :ensure (rainbow-csv :type git :host github
                       :repo "emacs-vs/rainbow-csv")
  :hook ((csv-mode . rainbow-csv-mode)
         (tsv-mode . rainbow-csv-mode))
  )

(use-package lsp-ltex-plus
  :defer t
  :ensure (lsp-ltex-plus :type git :host github
                         :repo "emacs-languagetool/lsp-ltex-plus")
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex-plus)
                       (lsp-deferred)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-plus-server-store-path "")
  (setq lsp-ltex-plus-version "18.5.1")  ; make sure you have set this, see below
  )  ; make sure you have set this, see below

(use-package jinx
  :ensure nil
  :defer t
  :config
  ;; Extra face(s) to ignore
  (setq jinx-languages "en-us")
  (push 'org-inline-src-block
        (alist-get 'org-mode jinx-exclude-faces))
  ;; Take over the relevant bindings.
  :general (
            [remap ispell-word] #'jinx-correct
            [remap evil-next-flyspell-error] #'jinx-next
            [remap evil-prev-flyspell-error] #'jinx-previous)
  :hook
  (text-mode . jinx-mode)
  (prog-mode . jinx-mode))

(use-package outline-indent
  :ensure t
  :defer t
  :commands outline-indent-minor-mode

  :hook
  (elpaca-after-init . outline-indent-minor-mode)
  :config
  (setopt outline-indent-default-offset 4)
  (setopt outline-indent-shift-width 4)
  :custom
  (outline-indent-ellipsis " ▼ "))

(use-package smartparens
  :ensure t
  :defer t
  :hook
  (elpaca-after-init . smartparens-global-mode)
  :config
  (sp-pair "`" "`"
           :actions '())
  )

;; (use-package easysession
;;   :ensure t
;;   :defer t
;;   :commands (easysession-switch-to
;;              easysession-save-as
;;              easysession-save-mode
;;              easysession-load-including-geometry)
;;   :custom
;;   ;; Interval between automatic session saves
;;   (easysession-save-interval (* 10 60))
;;   ;; Make the current session name appear in the mode-line
;;   (easysession-mode-line-misc-info t)
;;   :general (:prefix "SPC"
;;                     :keymaps 'override
;;                     :states 'normal
;;                     "l l" #'easysession-switch-to
;;                     "l s" #'easysession-save-as)
;;   :init
;;   (add-hook 'emacs-startup-hook #'easysession-load-including-geometry 102)
;;   (add-hook 'emacs-startup-hook #'easysession-save-mode 103))

(use-package savehist
  :ensure nil
  :config
  (setopt savehist-save-minibuffer-history t
          savehist-autosave-interval nil)
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'mark-ring)
  (add-to-list 'savehist-additional-variables 'search-ring)
  ;; (add-to-list 'savehist-additional-variables 'easysession--current-session-name)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring))

(use-package ws-butler
  :ensure t
  :defer t
  :hook (prog-mode . ws-butler-mode))


(use-package apheleia
  :ensure t
  :defer t
  :commands (apheleia-mode
             apheleia-global-mode)
  :hook
  (elpaca-after-init . apheleia-global-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package project
  :ensure nil
  :defer t
  :general (:prefix "SPC"
                    :keymaps 'override
                    :states 'normal
                    "p p" #'project-switch-project
                    )
  )



(use-package iimage
  :ensure nil
  :general (
            :keymaps 'image-mode-map
            :states 'normal
            "w" #'image-transform-fit-to-window
            "R" #'image-rotate
            )
  )

(use-package ox-pandoc
  :after ox
  :ensure t)

(general-define-key
 :prefix "SPC"
 :states 'normal
 :keymaps 'override
 "x" #'scratch-buffer
 "X" #'org-capture
 "f F" #'switch-to-buffer-other-frame
 "f W" #'switch-to-buffer-other-window
 )

(general-define-key :prefix "SPC g"
                    :keymaps 'override
                    :states 'normal
                    "g" #'magit
                    "t" #'git-timemachine
                    )

(general-define-key
 :prefix "SPC o"
 :states 'normal
 :keymaps 'override
 :desc "Org agenda"       "A"  #'org-agenda
 :desc "Agenda"         "a a"  #'org-agenda
 :desc "Todo list"      "a t"  #'org-todo-list
 :desc "Tags search"    "a m"  #'org-tags-view
 :desc "View search"    "a v"  #'org-search-view
 :desc "mu4e"    "m"  #'mu4e
 :desc "Default browser"    "b"  #'browse-url-of-file
 :desc "Start debugger"     "d"  #'+debugger/start
 :desc "New frame"          "f"  #'make-frame
 :desc "Select frame"       "F"  #'select-frame-by-name
 :desc "REPL"               "r"  #'+eval/open-repl-other-window
 :desc "REPL (same window)" "R"  #'+eval/open-repl-same-window
 :desc "Dired"              "-"  #'dired-jump
 :desc "Open directory in dirvish"    "/" #'dirvish
 :desc "Project sidebar"              "p" #'dirvish-side
 :desc "vterm"              "t" #'multi-vterm
 )

(general-define-key
 :states 'normal
 "K" #'xref-find-apropos
 )

(general-define-key
 :prefix "SPC t"
 :states 'normal
 :keymaps 'override
 :desc "toggle code wrapping"              "w"   #'visual-line-mode
 :desc "toggle flymake"              "f"   #'flymake-mode
 )

(general-define-key
 :prefix "SPC c"
 :states 'normal
 :keymaps 'override
 :desc "LSP Execute code action"              "a"   #'lsp-execute-code-action
 :desc "LSP Organize imports"                 "o"   #'lsp-organize-imports
 :desc "LSP Rename"                           "r"   #'lsp-rename
 :desc "Symbols"                              "S"   #'lsp-treemacs-symbols
 :desc "Jump to symbol in current workspace" "j"   #'consult-lsp-symbols

 )

(use-package dumb-jump
  :ensure t
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setopt xref-show-definitions-function #'xref-show-definitions-completing-read)
  :config
  (setopt dumb-jump-force-searcher 'rg)
  )

(setopt user-full-name "Leo Aparisi de Lannoy")
(setopt treesit-font-lock-level 4)
(setopt auto-save-default t)

(setopt auto-save-interval 300)
(setopt auto-save-timeout 10)
(setopt auto-save-visited-interval 5)   ; Save after 5 seconds if inactivity

(setopt delete-by-moving-to-trash t)
(setopt imagemagick-render-type 1)
(setopt browse-url-chrome-program "brave")
(setopt display-line-numbers-type 'relative)
(setq-default tab-width 4)
(setopt dired-vc-rename-file t)
(setopt xref-search-program 'ripgrep
        )

(add-hook 'after-init-hook (lambda ()
                             (setenv "PATH"
                                     (concat
                                      "/Library/TeX/texbin/" path-separator
                                      (getenv "PATH")))
                             (add-to-list 'exec-path "/Library/TeX/texbin/")
                             )
          )


(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :ensure t
  :after latex
  :config
  (package-initialize)
  (pdf-tools-install)
  (setopt pdf-view-display-size 'fit-page)
  :general
  ([remap pdf-view-midnight-minor-mode] #'pdf-view-themed-minor-mode
   ))

(use-package saveplace-pdf-view
  :ensure t
  :after pdf-tools)

(use-package lsp-nix
  :ensure nil
  :after lsp-mode
  :defer t
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

(use-package nix-mode
  :defer t
  :hook (nix-mode . lsp-deferred)
  :ensure t)

(use-package ultra-scroll
  :ensure (ultra-scroll :type git :host github :repo "jdtsmith/ultra-scroll")
  :defer t
  :init
  (setopt scroll-conservatively 101 ; important!
          scroll-margin 0)
  :hook
  (elpaca-after-init . ultra-scroll-mode))

(defun efs/display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract elpaca-after-init-time before-init-time)))
   gcs-done))


(add-hook 'emacs-startup-hook #'efs/display-startup-time)
(setopt major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package emacs-everywhere
  :defer t
  :ensure t)

(use-package empv
  :ensure (empv :type git :host github :repo "isamert/empv.el")
  :defer t
  :autoload (empv--select-action)
  :config
  (with-eval-after-load 'embark (empv-embark-initialize-extra-actions))
  (setopt empv-allow-insecure-connections t)
  (setopt empv-youtube-use-tabulated-results t)
  (add-to-list 'empv-mpv-args "--ytdl-format=bestvideo+bestaudio/best[ext=mp4]/best")
  (add-to-list 'empv-mpv-args "--save-position-on-quit")
  (setopt empv-reset-playback-speed-on-quit t)
  (add-hook 'empv-init-hook #'empv-override-quit-key)
  )

;; (use-package auctex-latexmk
;;   :ensure t
;;   :defer t)
(use-package reftex
  :ensure nil
  :defer t
  :hook (LaTeX-mode . reftex-mode)
  :config
  ;; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992.
  (setopt reftex-cite-format
          '((?a . "\\autocite[]{%l}")
            (?b . "\\blockcquote[]{%l}{}")
            (?c . "\\cite[]{%l}")
            (?f . "\\footcite[]{%l}")
            (?n . "\\nocite{%l}")
            (?p . "\\parencite[]{%l}")
            (?s . "\\smartcite[]{%l}")
            (?t . "\\textcite[]{%l}"))
          reftex-plug-into-AUCTeX t
          reftex-toc-split-windows-fraction 0.3
          ;; This is needed when `reftex-cite-format' is set. See
          ;; https://superuser.com/a/1386206
          LaTeX-reftex-cite-format-auto-activate nil)
  (add-hook 'reftex-mode-hook #'evil-normalize-keymaps)
  )

(use-package auctex
  :ensure (auctex :repo "https://git.savannah.gnu.org/git/auctex.git" :branch "main"
                  :pre-build (("make" "elpa"))
                  :build (:not elpaca--compile-info) ;; Make will take care of this step
                  :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                  :version (lambda (_) (require 'auctex) AUCTeX-version))
  :hook ((LaTeX-mode . LaTeX-preview-setup)
         (LaTeX-mode . TeX-fold-mode)
         (LaTeX-mode . prettify-symbols-mode)
         )
  :defer t
  :config
  (setopt TeX-parse-self t ; parse on load
          TeX-auto-save t  ; parse on save
          ;; Use hidden directories for AUCTeX files.
          TeX-auto-local ".auctex-auto"
          TeX-style-local ".auctex-style"
          TeX-source-correlate-mode t
          TeX-source-correlate-method 'synctex
          ;; Don't start the Emacs server when correlating sources.
          TeX-source-correlate-start-server nil
          ;; Automatically insert braces after sub/superscript in `LaTeX-math-mode'.
          TeX-electric-sub-and-superscript t
          ;; Just save, don't ask before each compilation.
          TeX-save-query nil
          TeX-show-compilation t
          TeX-command-extra-options "-shell-escape")
  (setopt TeX-fold-auto-reveal t)
  (setq-default TeX-engine 'xetex)
  (setopt TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq-default preview-scale 1.6
                preview-scale-function
                (lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))
  ;; Don't cache preamble, it creates issues with SyncTeX. Let users enable
  ;; caching if they have compilation times that long.
  (setopt preview-auto-cache-preamble nil)

  ;; (require 'auctex-latexmk)
  ;; (auctex-latexmk-setup)
  )

(use-package evil-tex
  :ensure t
  :hook (LaTeX-mode . evil-tex-mode)
  :after (evil auctex)
  :defer t)

(use-package lsp-latex
  ;; this uses texlab
  :ensure t
  :defer t
  :hook ((LaTeX-mode . (lambda ()
                         (require 'lsp-latex)
                         (lsp)))
         (bibtex-mode . (lambda ()
                          (require 'lsp-latex)
                          (lsp-deferred)))
         )
  :config
  ;; Setting for pdf-tools
  (setopt lsp-latex-forward-search-executable "emacsclient")
  (setopt lsp-latex-forward-search-args
          '("--eval"
            "(lsp-latex-forward-search-with-pdf-tools \"%f\" \"%p\" \"%l\")"))
  )

(use-package treesit-fold
  :ensure (treesit-fold :type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :hook
  (elpaca-after-init . global-treesit-fold-mode)
  :defer t)

(use-package treesit-auto
  :defer t
  :hook (elpaca-after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  )

(use-package citar
  :ensure t
  :defer t
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :custom
  (org-cite-global-bibliography '("~/bib/references.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (defvar citar-indicator-notes-icons
    (citar-indicator-create
     :symbol (nerd-icons-mdicon
              "nf-md-notebook"
              :face 'nerd-icons-blue
              :v-adjust -0.3)
     :function #'citar-has-notes
     :padding "  "
     :tag "has:notes"))

  (defvar citar-indicator-links-icons
    (citar-indicator-create
     :symbol (nerd-icons-octicon
              "nf-oct-link"
              :face 'nerd-icons-orange
              :v-adjust -0.1)
     :function #'citar-has-links
     :padding "  "
     :tag "has:links"))

  (defvar citar-indicator-files-icons
    (citar-indicator-create
     :symbol (nerd-icons-faicon
              "nf-fa-file"
              :face 'nerd-icons-green
              :v-adjust -0.1)
     :function #'citar-has-files
     :padding "  "
     :tag "has:files"))

  (setopt citar-indicators
          (list citar-indicator-files-icons
                citar-indicator-notes-icons
                citar-indicator-links-icons))
  ;; optional: org-cite-insert is also bound to C-c C-x C-@
  :bind
  (:map org-mode-map :package org ("C-c b" . #'org-cite-insert)))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package oc-csl-activate
  :ensure (oc-csl-activate :type git :host github :repo "andras-simonyi/org-cite-csl-activate")
  :after citar org
  :init
  (setopt org-cite-activate-processor 'csl-activate)
  :config
  (require 'oc-csl-activate)
  (setopt org-cite-csl-activate-use-document-style t)
  (setopt org-cite-csl-activate-use-document-locale t)
  (setopt org-cite-csl-activate-use-citar-cache t)
  )

(use-package org-noter
  :ensure t
  :defer t
  :preface
  ;; Allow the user to preempt this and set the document search path
  ;; If not set then use `org-directory'
  (defvar org-noter-notes-search-path nil)
  :config
  (unless org-noter-notes-search-path
    (setopt org-noter-notes-search-path (list org-directory)))
  (setopt org-noter-auto-save-last-location t
          org-noter-separate-notes-from-heading t))

(use-package edraw-org
  :defer t
  :after org
  :ensure (edraw-org :type git :host github :repo "misohena/el-easydraw")
  :config
  (edraw-org-setup-default)
  )

(use-package nov
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  )

(use-package dape
  :ensure t
  :defer t
  :preface
  ;; By default dape shares the same keybinding prefix as `gud'
  ;; If you do not want to use any prefix, set it to nil.
  ;; (setopt dape-key-prefix "\C-x\C-a")

  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)

  :config
  ;; Persist breakpoints after closing DAPE.
  (dape-breakpoint-global-mode +1)
  (add-hook 'dape-start-hook #'dape-breakpoint-load 0)
  (add-hook 'dape-stopped-hook #'dape-breakpoint-save 'append)


  ;; Info buffers to the right
  (setopt dape-buffer-window-arrangement 'right)

  ;; Info buffers like gud (gdb-mi)
  ;; (setopt dape-buffer-window-arrangement 'gud)
  ;; (setopt dape-info-hide-mode-line nil)

  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; Showing inlay hints
  (setopt dape-inlay-hints t)

  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook 'kill-buffer)

  ;; Projectile users
  ;; (setopt dape-cwd-function 'projectile-project-root)
  )

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (setq markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-gfm-additional-languages '("sh")
        markdown-make-gfm-checkboxes-buttons t
        markdown-fontify-whole-heading-line t
        markdown-fontify-code-blocks-natively t)

  :config
  (sp-local-pair '(markdown-mode gfm-mode) "`" "`"
                 :unless '(:add sp-point-before-word-p sp-point-before-same-p))

  (add-to-list 'markdown-code-lang-modes '("rust" . rustic-mode)))


(use-package evil-markdown
  :ensure (evil-markdown :type git :host github :repo "Somelauw/evil-markdown")
  :defer t
  :hook (markdown-mode . evil-markdown-mode)
  :config
  (add-hook 'evil-markdown-mode-hook #'evil-normalize-keymaps)
  )
