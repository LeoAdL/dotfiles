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
(add-hook 'elpaca-after-init-hook #'savehist-mode)

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(add-hook 'elpaca-after-init-hook #'save-place-mode)
(use-package general :ensure (:wait t) :demand t
 :config
 (general-auto-unbind-keys)
 (general-evil-setup)
 )

(use-package vterm
  :ensure t
  :defer t
  :hook (vterm-mode . hide-mode-line-mode) ; modeline serves no purpose in vterm
  :commands vterm
  :general (:prefix "SPC"
            :states 'normal
            "o t" #'vterm)
  :config
  ;; Speed up vterm
  (setq vterm-kill-buffer-on-exit t)

  ;; 5000 lines of scrollback, instead of 1000
  (setq vterm-max-scrollback 5000)
  (setq vterm-timer-delay 0.01))
;; Tip: You can remove the `vertico-mode' use-package and replace it
;;      with the built-in `fido-vertical-mode'.
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :defer t
  :commands vertico-mode
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
  (completion-category-overrides '((file (styles partial-completion)))))

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
         "f b" #'consult-buffer
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
         "G f" #'consult-flymake
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
  (setq consult-narrow-key "<"))

(use-package consult-dir
  :ensure t
  :general (:prefix "SPC"
            :states 'normal
            "f d" #'consult-dir)
  )

(setq evil-want-keybinding nil)

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
  (evil-collection-init))
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

(use-package evil-textobj-tree-sitter
 :ensure t
  :defer t
  :after treesit 
  :config

  (evil-define-key '(visual operator)
    "i" evil-textobj-tree-sitter-inner-text-objects-map
    "a" evil-textobj-tree-sitter-outer-text-objects-map)
  (evil-define-key 'normal
    "[g" evil-textobj-tree-sitter-goto-previous-map
    "]g" evil-textobj-tree-sitter-goto-next-map)

  (map! (:map evil-textobj-tree-sitter-inner-text-objects-map
         "A" (evil-textobj-tree-sitter-get-textobj ("parameter.inner" "call.inner"))
         "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
         "F" (evil-textobj-tree-sitter-get-textobj "call.inner")
         "C" (evil-textobj-tree-sitter-get-textobj "class.inner")
         "v" (evil-textobj-tree-sitter-get-textobj "conditional.inner")
         "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
        (:map evil-textobj-tree-sitter-outer-text-objects-map
         "A" (evil-textobj-tree-sitter-get-textobj ("parameter.outer" "call.outer"))
         "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
         "F" (evil-textobj-tree-sitter-get-textobj "call.outer")
         "C" (evil-textobj-tree-sitter-get-textobj "class.outer")
         "c" (evil-textobj-tree-sitter-get-textobj "comment.outer")
         "v" (evil-textobj-tree-sitter-get-textobj "conditional.outer")
         "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))

        (:map evil-textobj-tree-sitter-goto-previous-map
         "a" (evil-textobj-tree-sitter-goto-textobj "parameter.outer" t)
         "f" (evil-textobj-tree-sitter-goto-textobj "function.outer" t)
         "F" (evil-textobj-tree-sitter-goto-textobj "call.outer" t)
         "C" (evil-textobj-tree-sitter-goto-textobj "class.outer" t)
         "c" (evil-textobj-tree-sitter-goto-textobj "comment.outer" t)
         "v" (evil-textobj-tree-sitter-goto-textobj "conditional.outer" t)
         "l" (evil-textobj-tree-sitter-goto-textobj "loop.outer" t))
        (:map evil-textobj-tree-sitter-goto-next-map
         "a" (evil-textobj-tree-sitter-goto-textobj "parameter.outer")
         "f" (evil-textobj-tree-sitter-goto-textobj "function.outer")
         "F" (evil-textobj-tree-sitter-goto-textobj "call.outer")
         "C" (evil-textobj-tree-sitter-goto-textobj "class.outer")
         "c" (evil-textobj-tree-sitter-goto-textobj "comment.outer")
         "v" (evil-textobj-tree-sitter-goto-textobj "conditional.outer")
         "l" (evil-textobj-tree-sitter-goto-textobj "loop.outer"))))

(use-package vimish-fold
:ensure t 
:after evil )

(use-package evil-vimish-fold
  :ensure t
  :after vimish-fold
  :init
    (global-evil-vimish-fold-mode 1)
  )

(use-package adaptive-wrap
:ensure t)


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
:general (
:prefix "SPC"
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
  (global-corfu-mode))

(use-package org-block-capf
:ensure (org-block-capf  :type git :host github :repo "xenodium/org-block-capf")
:after org
:hook (org-mode . org-block-capf-add-to-completion-at-point-functions))

(use-package cape
  :ensure t
  :defer t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
   (setq cape-dabbrev-min-length 2)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-history)
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
  :config
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


(display-time-mode)
(show-paren-mode +1)  ; Paren match highlighting
(winner-mode 1)
(pixel-scroll-precision-mode 1)

(setq user-full-name "Leo Aparisi de Lannoy"
      user-mail-address "leoaparisi@gmail.com")

(use-package doom-modeline
  :ensure t
  :defer t
  :config
  (setq doom-modeline-hud t)
  (setq doom-modeline-unicode-fallback t)
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
:general (
:prefix "SPC"
 :states 'normal
 "." #'dirvish)
:config
(setq dirvish-attributes'(vc-state subtree-state nerd-icons git-msg file-time file-size))
  (setq dirvish-default-layout '(0 0.4 0.6))
)

(use-package hl-todo
  :ensure t
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
  :hook ((prog-mode . indent-bars-mode)
         (shell-mode . indent-bars-mode)
         (eshell-mode . indent-bars-mode))
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
    indent-bars-color '(highlight :face-bg t :blend 0.15)
    indent-bars-no-stipple nil
    indent-bars-pattern "."
    indent-bars-width-frac 0.3
    indent-bars-pad-frac 0.1
    indent-bars-zigzag nil
    indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
    indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
    indent-bars-display-on-blank-lines t))

(use-package ligature
  :ensure t
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                       "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                       "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                       ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package diff-hl
:ensure t
:config
(global-diff-hl-mode))

(setq elpaca-ignored-dependencies
      (append
       ;; Downgrades-in-disguise
       '(cl-lib cl-generic nadvice use-package bind-key)
       ;; No repo, or repo is the GNU Emacs repo (250 MB download)
       (cl-loop
        for dep in elpaca-ignored-dependencies
        when (let ((repo (plist-get (elpaca-recipe dep) :repo)))
               (or (not repo)
                   (equal repo "https://github.com/emacs-mirror/emacs")))
        collect dep)))
(use-package magit
:ensure t
:general (:prefix "SPC"
            :states 'normal
            "g g" #'magit)
)

(use-package flymake-popon
:after flymake 
:ensure t)

(use-package lsp-mode
:ensure t
  :init
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)

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
:after (org evil)
:ensure t)

(use-package orgit
:after (org magit)
:ensure t)

(use-package jupyter
:defer t
:ensure t)

(use-package mu4e-compat
:after mu4e 
:ensure (mu4e-compat :type git :host github :repo "tecosaur/mu4e-compat"))

(use-package org-msg
:after (org mu4e)
:ensure t)

(setq pixel-scroll-precision-mode t)
(setq treesit-font-lock-level 4)
(setq auto-save-timeout 10)
(setq delete-by-moving-to-trash t)
(setq scroll-margin 0)
(setq scroll-conservatively 101)
(setq imagemagick-render-type 1)
(setq browse-url-chrome-program "brave")
(setq display-line-numbers-type 'relative)
(setq-default tab-width 4)

(add-to-list 'default-frame-alist
             '(font . "Iosevka:pixelsize=14"))
(global-display-line-numbers-mode)

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(general-define-key
 :keymaps '(normal insert emacs)
 "C-=" #'global-text-scale-adjust)

(use-package org 
:ensure t
:config 

(setq org-directory "~/org/")
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
)



(use-package org-modern
:ensure t
  :after org
  :hook ((org-mode . org-modern-mode) (org-agenda-finalize . org-modern-agenda))
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-hide-stars nil
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-block-name t
        org-modern-progress t
        org-modern-horizontal-rule t
        org-modern-keyword t))

 (use-package org-superstar
:ensure t
   :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package org-appear
        :ensure t
        :hook (org-mode . org-appear-mode)
        :config
        (setq org-appear-autoemphasis t
                org-appear-autosubmarkers t
                org-appear-autolinks t
                org-appear-autokeywords t
                org-appear-autoentities t
                org-appear-inside-latex t
                org-appear-autosubmarkers t))
