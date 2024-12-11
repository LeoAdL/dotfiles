;;; post-init.el --- DESCRIPTION -*- no-byte-compile: t; lexical-binding: t; -*-
;; Ensure Emacs loads the most recent byte-compiled files.
(setq load-prefer-newer t)

;; Ensure JIT compilation is enabled for improved performance by
;; native-compiling loaded .elc files asynchronously
(setq native-comp-jit-compilation t)
(setq native-comp-deferred-compilation t) ; Deprecated in Emacs > 29.1

(use-package compile-angel
  :ensure t
  :demand t
  :config
  ;; Ensure that quitting only occurs once Emacs finishes native compiling,
  ;; preventing incomplete or leftover compilation files in `/tmp`.
  (setq native-comp-async-query-on-exit t)
  (setq confirm-kill-processes t)
  (setq package-native-compile t)
  (compile-angel-on-load-mode)
  (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode))

;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(add-hook 'elpaca-after-init-hook #'global-auto-revert-mode)

;; recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(add-hook 'elpaca-after-init-hook #'recentf-mode)

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'elpaca-after-init-hook #'save-place-mode)


(use-package general :ensure (:wait t) :demand t
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
  :defer t
  :ensure t
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
  (setq org-directory "~/org/")
  (setq org-hide-emphasis-markers t)
  (setq org-preview-latex-image-directory "~/.cache/ltximg/")
  ;; ORG LATEX PREVIEW
  (setq org-startup-with-latex-preview t)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent"))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2))
  (setq org-preview-latex-default-process 'dvipng)
  (setq
   org-agenda-files (list org-directory)                  ; Seems like the obvious place.
   org-use-property-inheritance t                         ; It's convenient to have properties inherited.
   org-log-done 'time                                     ; Having the time a item is done sounds convenient.
   org-list-allow-alphabetical t                          ; Have a. A. a) A) list bullets.
   org-catch-invisible-edits 'smart                       ; Try not to accidently do weird stuff in invisible regions.
   org-export-with-sub-superscripts '{}                   ; Don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}.
   org-export-allow-bind-keywords t                       ; Bind keywords can be handy
   org-image-actual-width '(0.9))
  (setq org-babel-default-header-args
        '((:session . "none")
          (:results . "replace")
          (:exports . "code")
          (:cache . "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:comments . "link")))

  (setq org-agenda-skip-scheduled-if-done nil
        org-agenda-skip-deadline-if-done nil
        org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────")

  (setq org-src-fontify-natively t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-startup-with-inline-images t
        org-startup-indented t
        ;; Org styling, hide markup etc.
        org-pretty-entities t
        org-hide-leading-stars t
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . 'nerd-icons-red)
          (?B . 'nerd-icons-orange)
          (?C . 'nerd-icons-yellow)
          (?D . 'nerd-icons-green)
          (?E . 'nerd-icons-blue)))
  (setq org-highlight-latex-and-related '(native script entities))
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
  (setq org-capture-templates
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
  (setq vterm-kill-buffer-on-exit t)

  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000)
  (setq vterm-timer-delay 0.01))
(use-package multi-vterm
  :ensure t
  :after vterm
  :config
  (add-hook 'vterm-mode-hook
			(lambda ()
			  (setq-local evil-insert-state-cursor 'box)
			  (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil)
  (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

;; Tip: You can remove the `vertico-mode' use-package and replace it
;;      with the built-in `fido-vertical-mode'.
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
  :config
  (defun +embark-live-vertico ()
    "Shrink Vertico minibuffer when `embark-live' is active."
    (when-let (win (and (string-prefix-p "*Embark Live" (buffer-name))
                        (active-minibuffer-window)))
      (with-selected-window win
        (when (and (bound-and-true-p vertico--input)
                   (fboundp 'vertico-multiform-unobtrusive))
          (vertico-multiform-unobtrusive)))))

  (add-hook 'embark-collect-mode-hook #'+embark-live-vertico)
  :hook (elpaca-after-init . vertico-mode))

(use-package nerd-icons-completion
  :after marginalia
  :ensure t
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

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
  :after vertico
  :defer t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
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
  ;; Optionally configure the register formatting. This improves the register
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
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
  (defun consult-ripgrep-up-directory ()
    (interactive)
    (let ((parent-dir (file-name-directory (directory-file-name default-directory))))
      (when parent-dir
        (run-at-time 0 nil
                     #'consult-ripgrep
                     parent-dir
                     (ignore-errors
                       (buffer-substring-no-properties
                        (1+ (minibuffer-prompt-end)) (point-max))))))
    (minibuffer-quit-recursive-edit))

  (consult-customize
   consult-ripgrep
   :keymap (let ((map (make-sparse-keymap)))
             (define-key map (kbd "M-l") #'consult-ripgrep-up-directory)
             map))
  (setq consult-narrow-key "<")
  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))

  ;; OPTION 1: Activate globally for all consult-grep/ripgrep/find/...
  (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)

  )

(use-package consult-dir
  :ensure t
  :general (:prefix "SPC"
                    :keymaps 'override
                    :states 'normal
                    "f d" #'consult-dir)
  )

(use-package evil
  :ensure t
  :init
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :custom
  ( evil-want-C-u-scroll t)
  ( evil-want-fine-undo t)
  (evil-want-Y-yank-to-eol t)
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init)
  )
(use-package evil-args
  :ensure t
  :after evil)

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces))

(use-package evil-anzu
  :after evil
  :ensure (evil-anzu :type git :host github :repo "emacsorphanage/evil-anzu")
  :init
  (global-anzu-mode)
  )

(use-package vimish-fold
  :ensure t
  :after evil )

(use-package evil-vimish-fold
  :ensure t
  :after vimish-fold
  :init
  (global-evil-vimish-fold-mode 1)
  )

(use-package visual-fill-column
  :ensure t)

(use-package ibuffer-vc
  :ensure t
  )
(use-package undo-fu
  :ensure t
  :commands (undo-fu-only-undo
             undo-fu-only-redo
             undo-fu-only-redo-all
             undo-fu-disable-checkpoint)
  :custom
  ;; 3 times the default values
  (undo-limit (* 3 160000))
  (undo-strong-limit (* 3 240000)))

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode)
  (setq undo-fu-session-compression 'zst)
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
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t)
  )

(use-package ts-fold
  :ensure (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :after tree-sitter
  :config
  (global-ts-fold-mode +1))

(use-package vdiff
  :ensure t
  :defer t
  :commands (vdiff-buffers
             vdiff-buffers3
             vdiff-quit
             vdiff-files
             vdiff-files3)
  :custom
  (vdiff-auto-refine t)
  (vdiff-only-highlight-refinements t))

(use-package evil-visualstar
  :after evil
  :ensure t
  :defer t
  :commands global-evil-visualstar-mode
  :hook (elpaca-after-init . global-evil-visualstar-mode))
(use-package evil-surround
  :after evil
  :ensure t
  :defer t
  :commands global-evil-surround-mode
  :custom
  (evil-surround-pairs-alist
   '((?\( . ("(" . ")"))
     (?\[ . ("[" . "]"))
     (?\{ . ("{" . "}"))

     (?\) . ("(" . ")"))
     (?\] . ("[" . "]"))
     (?\} . ("{" . "}"))

     (?< . ("<" . ">"))
     (?> . ("<" . ">"))))
  :hook (elpaca-after-init . global-evil-surround-mode))
(with-eval-after-load "evil"
  (evil-define-operator my-evil-comment-or-uncomment (beg end)
    "Toggle comment for the region between BEG and END."
    (interactive "<r>")
    (comment-or-uncomment-region beg end))
  (evil-define-key 'normal 'global (kbd "gc") 'my-evil-comment-or-uncomment))
(use-package evil-snipe
  :defer t
  :commands evil-snipe-mode
  :hook (elpaca-after-init . evil-snipe-mode))

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)

  ;; Enable Corfu
  :config
  (setq corfu-quit-no-match t)
  (setq corfu-auto t)
  (global-corfu-mode +1))

(use-package org-block-capf
  :ensure (org-block-capf  :type git :host github :repo "xenodium/org-block-capf")
  :after org
  :hook (org-mode . org-block-capf-add-to-completion-at-point-functions))

(use-package cape-keyword
  :ensure (cape-keyword :type git :host github :repo "minad/cape")
  :config
  (add-to-list 'cape-keyword-list '(q-mode
                                    "abs" "cor" "ej" "gtime" "like" "mins" "prev" "scov" "system" "wavg" "acos" "cos" "ema" "hclose" "lj" "ljf" "mmax" "prior" "sdev" "tables" "where" "aj" "aj0" "count" "enlist" "hcount" "load" "mmin" "rand" "select" "tan" "while" "ajf" "ajf0" "cov" "eval" "hdel" "log" "mmu" "rank" "set" "til" "within" "all" "cross" "except" "hopen" "lower" "mod" "ratios" "setenv" "trim" "wj" "wj1" "and" "csv" "exec" "hsym" "lsq" "msum" "raze" "show" "type" "wsum" "any" "cut" "exit" "iasc" "ltime" "neg" "read0" "signum" "uj" "ujf" "xasc" "asc" "delete" "exp" "idesc" "ltrim" "next" "read1" "sin" "ungroup" "xbar" "asin" "deltas" "fby" "if" "mavg" "not" "reciprocal" "sqrt" "union" "xcol" "asof" "desc" "fills" "ij" "ijf" "max" "null" "reval" "ss" "update" "xcols" "atan" "dev" "first" "in" "maxs" "or" "reverse" "ssr" "upper" "xdesc" "attr" "differ" "fkeys" "insert" "mcount" "over" "rload" "string" "upsert" "xexp" "avg" "distinct" "flip" "inter" "md5" "parse" "rotate" "sublist" "value" "xgroup" "avgs" "div" "floor" "inv" "mdev" "peach" "rsave" "sum" "var" "xkey" "bin" "binr" "do" "get" "key" "med" "pj" "rtrim" "sums" "view" "xlog" "ceiling" "dsave" "getenv" "keys" "meta" "prd" "save" "sv" "views" "xprev" "cols" "each" "group" "last" "min" "prds" "scan" "svar" "vs" "xrank" ".Q.ajf0" ".Q.sx" ".Q.k" ".Q.K" ".Q.host" ".Q.addr" ".Q.gc" ".Q.ts" ".Q.gz" ".Q.w" ".Q.res" ".Q.addmonths" ".Q.f" ".Q.fmt" ".Q.ff" ".Q.fl" ".Q.opt" ".Q.def" ".Q.ld" ".Q.qt" ".Q.v" ".Q.qp" ".Q.V" ".Q.ft" ".Q.ord" ".Q.nv" ".Q.tx" ".Q.tt" ".Q.fk" ".Q.t" ".Q.ty" ".Q.nct" ".Q.fu" ".Q.fc" ".Q.A" ".Q.a" ".Q.n" ".Q.nA" ".Q.an" ".Q.b6" ".Q.Aa" ".Q.unm" ".Q.id" ".Q.j10" ".Q.x10" ".Q.j12" ".Q.x12" ".Q.btoa" ".Q.sha1" ".Q.prf0" ".Q.objp" ".Q.lo" ".Q.l" ".Q.sw" ".Q.tab" ".Q.t0" ".Q.s1" ".Q.s2" ".Q.S" ".Q.s" ".Q.hap" ".Q.hmb" ".Q.hg" ".Q.hp" ".Q.a1" ".Q.a0" ".Q.IN" ".Q.qa" ".Q.qb" ".Q.vt" ".Q.bvfp" ".Q.bvi" ".Q.bv" ".Q.sp" ".Q.pm" ".Q.pt" ".Q.MAP" ".Q.dd" ".Q.d0" ".Q.p1" ".Q.p2" ".Q.p" ".Q.view" ".Q.jp" ".Q.rp" ".Q.fobj" ".Q.L1" ".Q.L" ".Q.li" ".Q.cn" ".Q.pcnt" ".Q.dt" ".Q.ind" ".Q.fp" ".Q.foo" ".Q.a2" ".Q.qd" ".Q.xy" ".Q.x1" ".Q.x0" ".Q.x2" ".Q.ua" ".Q.q0" ".Q.qe" ".Q.ps" ".Q.enxs" ".Q.enx" ".Q.en" ".Q.ens" ".Q.par" ".Q.dpts" ".Q.dpt" ".Q.dpfts" ".Q.dpft" ".Q.hdpf" ".Q.fsn" ".Q.fs" ".Q.fpn" ".Q.fps" ".Q.dsftg" ".Q.M" ".Q.chk" ".Q.Ll" ".Q.Lp" ".Q.Lx" ".Q.Lu" ".Q.Ls" ".Q.fqk" ".Q.fql" ".Q.btx" ".Q.bt" ".Q.sbt" ".Q.trp" ".Q.trpd" ".Q.dr" ".Q.dw" ".Q.pl0" ".Q.pl" ".Q.jl8" ".Q.srr" ".Q.prr" ".Q.lu" ".Q.DL" ".Q.dbg" ".Q.err" ".Q.BP" ".Q.bp" ".Q.bs" ".Q.bu" ".Q.bd" ".Q.bc" ".h.htc" ".h.hta" ".h.htac" ".h.ha" ".h.hb" ".h.pre" ".h.xmp" ".h.d" ".h.cd" ".h.td" ".h.hc" ".h.xs" ".h.xd" ".h.ex" ".h.iso8601" ".h.eb" ".h.es" ".h.ed" ".h.edsn" ".h.ec" ".h.tx" ".h.xt" ".h.ka" ".h.c0" ".h.c1" ".h.logo" ".h.sa" ".h.html" ".h.sb" ".h.fram" ".h.jx" ".h.uh" ".h.sc" ".h.hug" ".h.hu" ".h.ty" ".h.hnz" ".h.hn" ".h.HOME" ".h.hy" ".h.hp" ".h.he" ".h.val" ".h.br" ".h.hr" ".h.nbr" ".h.code" ".h.http" ".h.text" ".h.data" ".h.ht" ".j.e" ".j.q" ".j.s" ".j.es" ".j.J" ".j.k" ".j.jd" ".j.j"
                                    ))
  )
(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (setq cape-dabbrev-min-length 2)

  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  )
;; Hide warnings and display only errors
(setq warning-minimum-level :error)

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu-history
  :ensure (corfu-history :type git :host github :repo "minad/corfu")
  :hook (corfu-mode . corfu-history-mode)
  :after savehist
  :init
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package corfu-popupinfo
  :ensure (corfu-popupinfo :type git :host github :repo "minad/corfu")
  :hook (corfu-mode . corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0)))


;; Optionally:
(setq nerd-icons-corfu-mapping
      '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
        (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
        ;; ...
        (t :style "cod" :icon "code" :face font-lock-warning-face)))
;; Remember to add an entry for `t', the library uses that as default.


(use-package which-key
  :ensure t
  :init
  (which-key-mode))

;; Configure Emacs to ask for confirmation before exiting
(setq confirm-kill-emacs 'y-or-n-p)

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
  :config
  (setq doom-modeline-hud t)
  (setq doom-modeline-unicode-fallback t)
  (setq find-file-visit-truename t)
  ;; (setq nerd-icons-scale-factor 1)
  ;; (setq doom-modeline-height 1) ; optional
  (setq doom-modeline-project-detection 'project)
  (setq mode-line-right-align-edge 'right-fringe)

  ;; (custom-set-faces
  ;;  '(mode-line ((t (:family "Iosevka" :height .9))))
  ;;  '(mode-line-active ((t (:family "Iosevka" :height .9)))) ; For 29+
  ;;  '(mode-line-inactive ((t (:family "Iosevka" :height .9)))))

  :hook (elpaca-after-init . doom-modeline-mode))

(use-package catppuccin-theme
  :ensure t
  :defer t
  :init
  (load-theme 'catppuccin :no-confirm)
  )

(use-package dirvish
  :ensure t
  :defer t
  :init
  (dirvish-override-dired-mode)
  :general
  (:states 'normal
           :keymaps 'dirvish-mode-map
           "a"   #'dirvish-quick-access
           "q"   #'dirvish-quit
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
    (setq dired-use-ls-dired t
          insert-directory-program "/opt/homebrew/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first"))
  (setq dirvish-attributes'(vc-state subtree-state nerd-icons git-msg file-time file-size))
  (setq dirvish-default-layout '(0 0.4 0.6))
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
  (setq hl-todo-highlight-punctuation ":"
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
  (indent-bars-prefer-character nil)
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
  (setq
   indent-bars-color '(highlight :face-bg t :blend 0.05)
   indent-bars-no-stipple nil
   indent-bars-pattern "."
   indent-bars-width-frac 0.3
   indent-bars-pad-frac 0.1
   indent-bars-zigzag nil
   indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 0.95) ; blend=1: blend with BG only
   indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
   indent-bars-display-on-blank-lines t))

(use-package ligature
  :ensure t
  :defer t
  :init
  (global-ligature-mode +1)
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  )

(use-package diff-hl
  :ensure t
  :defer t
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode +1)
  (diff-hl-margin-mode))

(use-package transient
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :after transient
  :general (:prefix "SPC"
                    :keymaps 'override
                    :states 'normal
                    "g g" #'magit)
  )

(use-package magit-todos
  :ensure t
  :after magit
  :init (magit-todos-mode 1))

(use-package git-timemachine
  :defer t
  :ensure (git-timemachine :type git :host codeberg :repo "pidu/git-timemachine"))

(use-package flymake-popon
  :after flymake
  :ensure t
  :init
  (global-flymake-popon-mode +1))

(use-package flymake-vale
  :ensure (flymake-vale :type git :host github :repo "tpeacock19/flymake-vale")
  :defer t
  :after flymake
  :init
  (add-hook 'text-mode-hook #'flymake-vale-load)
  (add-hook 'latex-mode-hook #'flymake-vale-load)
  (add-hook 'org-mode-hook #'flymake-vale-load)
  (add-hook 'markdown-mode-hook #'flymake-vale-load)
  (add-hook 'message-mode-hook #'flymake-vale-load)
  )

(use-package lsp-mode
  :ensure t
  :general
  (:states 'normal
           :desc "Jump to definition"                    "g d"   #'xref-find-definitions
           :desc "Jump to references"                    "g r"   #'xref-find-references
           :desc "Jump to references"                    "g i"   #'lsp-find-implementation
           :desc "Jump to references"                    "g D"   #'lsp-find-declarations)
  :defer t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ;; Configure flex
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :config
  (setq lsp-completion-provider nil)
  (setq lsp-warn-no-matched-clients nil))

;; optionally
(use-package lsp-ui
  :after lsp
  :ensure t
  :commands lsp-ui-mode)

(use-package org-contrib
  :after org
  :ensure t)

(use-package ox-clip
  :after org
  :ensure t)

(use-package org-cliplink
  :after org
  :ensure t)

(use-package toc-org
  :after org
  :ensure t)


(use-package evil-org
  :after (org)
  :ensure (evil-org :type git :host github :repo "Somelauw/evil-org-mode")
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  )

(use-package orgit
  :after (org magit)
  :ensure t)

(use-package jupyter
  :defer t
  :ensure t
  :config
  (setq jupyter-use-zmq t
        jupyter-eval-use-overlays nil
        jupyter-eval-short-result-max-lines 0
        jupyter-eval-overlay-keymap "<backtab>"
        jupyter-default-notebook-port 8895)
  :bind (("<backtab>" . jupyter-eval-toggle-overlay)))

(use-package code-cells
  :defer t
  :ensure t)

(use-package treesit-auto
  :ensure t
  :defer t
  :after treesit
  :config
  (setq treesit-auto-install 'prompt
        treesit-auto-mode t))

(use-package evil-textobj-tree-sitter
  :after (evil treesit)
  :ensure t
  :defer t)

(when (string= system-type "darwin")
  (use-package mu4e
    :ensure nil
    :after org
    :load-path  "/opt/homebrew/share/emacs/site-lisp/mu/mu4e/"
    :config
    (setq sendmail-program (executable-find "msmtp")
          send-mail-function #'smtpmail-send-it
          message-sendmail-f-is-evil t
          mu4e-compose-complete-addresses t
          message-sendmail-extra-arguments '("--read-envelope-from")
          message-send-mail-function #'message-send-mail-with-sendmail
          mu4e-sent-messages-behavior 'sent ;; Save sent messages
          mu4e-headers-auto-update t                ; avoid to type `g' to update
          mml-secure-openpgp-signers '("6A5C039B63B86AC6C5109955B57BA04FBD759C7F" "D1D9947126EE64AC7ED3950196F352393B5B3C2E")
          mml-secure-openpgp-sign-with-sender t
          mu4e-use-fancy-chars t                   ; allow fancy icons for mail threads
          mu4e-change-filenames-when-moving t
          mu4e-index-lazy-check nil
          mu4e-search-results-limit 300
          mu4e-context-policy 'pick-first ;; Always ask which context to use when composing a new mail
          mu4e-compose-context-policy 'ask ;; Always ask which context to use when composing a new mail
          mu4e-update-interval (* 1.5 60)
          mu4e-mu-allow-temp-file t
          mu4e-headers-precise-alignment t
          mu4e-compose-complete-only-after "2015-01-01"
          message-dont-reply-to-names #'mu4e-personal-or-alternative-address-p
          mu4e-bookmarks '((:name "Unread messages" :query "flag:unread AND maildir:/.*inbox/" :key 117)
                           (:name "Today's messages" :query "date:today..now AND maildir:/.*inbox/" :key 116)
                           ("flag:flagged" "Flagged messages" 102)
                           (:name "Unified inbox" :query "maildir:/.*inbox/" :key 105)
                           (:name "Sent" :query "maildir:/.*Sent/" :key 115)
                           (:name "Drafts" :query "maildir:/.*Drafts/" :key 100)
                           (:name "Spam" :query "maildir:/.*Spam/ or maildir:/.*Junk/" :key 83)
                           (:name "Trash" :query "maildir:/.*Trash/" :key 84))
          mu4e-attachment-dir "~/Downloads"
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
    :init
    (add-hook 'completion-at-point-functions #'mu4e-complete-contact)
    (corfu-mode -1)
    (corfu-mode +1)
    )

  ;; (use-package mu4e-compat
  ;;   :after mu4e
  ;;   :ensure (mu4e-compat :type git :host github :repo "tecosaur/mu4e-compat"))

  (use-package org-msg
    :after (org mu4e)
    :hook (mu4e-compose-mode . org-msg-edit-mode)
    :ensure t
    :config
    (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil tex:dvipng"
          org-msg-startup "hidestars indent inlineimages"
          org-msg-greeting-name-limit 3
          org-msg-default-alternatives '((new . (html))
					                     (reply-to-html . (html)))
          org-msg-convert-citation t
          org-msg-signature "
,#+begin_signature
Leo Aparisi de Lannoy
,#+end_signature")))


(add-hook 'conf-mode-hook #'flymake-mode-on)
(add-hook 'prog-mode-hook #'flymake-mode-on)
(add-hook 'text-mode-hook #'flymake-mode-on)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

(add-to-list 'default-frame-alist
             '(font . "Iosevka:pixelsize=20:foundry=UKWN:weight=medium:slant=normal:width=normal:spacing=90:scalable=true
"))


(general-define-key
 :keymaps '(normal insert emacs)
 "C-=" #'global-text-scale-adjust)


(use-package ox-jira
  :ensure t
  :after org)

(use-package org-modern
  :ensure t
  :after org
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-hide-stars nil
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-block-name t
        org-modern-progress t
        org-modern-horizontal-rule t
        org-modern-keyword t))
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(use-package org-superstar
  :after org
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(use-package org-appear
  :ensure t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks t
        org-appear-autokeywords t
        org-appear-autoentities t
        org-appear-inside-latex nil
        org-appear-autosubmarkers t))

(use-package org-fragtog
  :ensure t
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package org-pandoc-import
  :ensure (org-pandoc-import :type git :host github
                             :repo "tecosaur/org-pandoc-import"
                             :files ("*.el" "filters" "preprocessors"))
  :after org
  :init
  (org-pandoc-import-backend jira))

(use-package q-mode
  :defer t
  :ensure t
  :config (setq
           q-program "q -s 7"))

(use-package vlf
  :ensure t
  :defer t
  :init
  (require 'vlf-setup)
  )

(use-package csv-mode
  :defer t
  :ensure (csv-mode :type git :host github :repo "emacsmirror/csv-mode":branch "master" )
  :hook ((csv-mode . csv-align-mode)
         (csv-mode . csv-header-line))
  )
(use-package rainbow-csv
  :ensure (rainbow-csv :type git :host github
                       :repo "emacs-vs/rainbow-csv")
  :hook ((csv-mode . rainbow-csv-mode)
         (tsv-mode . rainbow-csv-mode))
  )

(use-package lsp-ltex
  :after lsp-mode
  :ensure t
  :hook (text-mode . (lambda ()
                       (require 'lsp-ltex)
                       (lsp-deferred)))  ; or lsp-deferred
  :init
  (setq lsp-ltex-completion-enabled t)
  (setq lsp-ltex-version "16.0.0"))

(use-package jinx
  :ensure nil
  :defer t
  :config
  ;; Extra face(s) to ignore
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
  :config
  (setq outline-indent-default-offset 4)
  (setq outline-indent-shift-width 4)
  )

(use-package smartparens
  :ensure t
  :defer t
  :config
  (sp-pair "`" "`"
           :actions '())
  :init
  (smartparens-global-mode +1)
  )

(use-package easysession
  :ensure t
  :defer t
  :custom
  ;; Interval between automatic session saves
  (easysession-save-interval (* 10 60))
  ;; Make the current session name appear in the mode-line
  (easysession-mode-line-misc-info t)
  :general (:prefix "SPC"
                    :keymaps 'override
                    :states 'normal
                    "l l" #'easysession-switch-to
                    "l s" #'easysession-save-as)
  :init
  (add-hook 'emacs-startup-hook #'easysession-save-mode 99))

(use-package savehist
  :ensure nil
  :hook
  (elpaca-after-init . savehist-mode)
  :config
  (add-to-list 'savehist-additional-variables 'kill-ring)
  (add-to-list 'savehist-additional-variables 'mark-ring)
  (add-to-list 'savehist-additional-variables 'search-ring)
  (add-to-list 'savehist-additional-variables 'easysession--current-session-name)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring))

(use-package ws-butler
  :ensure t
  :defer t
  :hook (prog-mode . ws-butler-mode))

;; Configure Tempel
(use-package tempel
  :ensure t
  :defer t
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
  )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :ensure t
  :defer t)

(use-package apheleia
  :ensure t
  :defer t
  :init
  (apheleia-global-mode +1))

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

(use-package ox-pandoc
  :after org
  :ensure t)

;; (use-package solaire-mode
;;   :ensure t
;;   :defer t
;;   :init
;;   (solaire-global-mode +1))

(general-define-key
 :prefix "SPC"
 :states 'normal
 :keymaps 'override
 "x" #'scratch-buffer
 "X" #'org-capture
 "f F" #'switch-to-buffer-other-frame
 "f W" #'switch-to-buffer-other-window
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
 "K" #'lsp-ui-doc-glance
 )

(general-define-key
 :prefix "SPC t"
 :states 'normal
 :keymaps 'override
 :desc "toggle code wrapping"              "w"   #'visual-line-mode
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

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(setq user-full-name "Leo Aparisi de Lannoy")
(setq treesit-font-lock-level 4)
(setq auto-save-timeout 10)
(setq scroll-conservatively 101)

(setq delete-by-moving-to-trash t)
(setq imagemagick-render-type 1)
(setq browse-url-chrome-program "brave")
(setq display-line-numbers-type 'relative)
(setq-default tab-width 4)
(setq dired-vc-rename-file t)
(setq xref-search-program 'ripgrep)
(add-hook 'elpaca-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))

(add-hook 'elpaca-after-init-hook (lambda () (server-start)
                                    (show-paren-mode +1)  ; Paren match highlighting
                                    (winner-mode 1)
                                    (global-visual-line-mode +1)
                                    (pixel-scroll-precision-mode 1)
                                    ))

(use-package recentf
  :ensure nil
  :defer t
  :init
  (run-at-time nil 600 'recentf-save-list))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :ensure nil
  :general
  ([remap pdf-view-midnight-minor-mode] #'pdf-view-themed-minor-mode
   )
  :defer t)

(use-package saveplace-pdf-view
  :ensure t
  :after pdf-tools
  :init
  (save-place-mode 1)
  :defer t)

(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)

(use-package dumb-jump
  :ensure t
  :defer t
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-force-searcher 'rg)
  )
