;;; config.el -*- lexical-binding: t; -*-

;; brew tap railwaycat/emacsmacport
;; brew install emacs-mac --with-mac-metal --with-natural-title-bar --with-native-compilation --with-xwidget

(setq user-full-name "Leo Aparisi de Lannoy"
      user-mail-address "leoaparisi@gmail.com")

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(after! evil
(setq evil-want-fine-undo t))                       ; By default while in insert all changes are one big blob. Be more granular

(setq browse-url-chrome-program "brave")

(after! dirvish
   (setq dirvish-attributes'(vc-state subtree-state nerd-icons git-msg file-time file-size))
      (setq dirvish-default-layout '(0 0.4 0.6))
)

(after! doom-ui
(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-variable-pitch-font (font-spec :family "Iosevka Aile")
      doom-unicode-font (font-spec :family "Iosevka")
      doom-big-font (font-spec :family "Iosevka" :size 24)
      doom-serif-font (font-spec :family "Iosevka Aile" :weight 'light)))

;; (setq doom-theme 'doom-nord)
(after! doom-ui
(setq doom-theme `doom-nord)
)

;; (setq fancy-splash-image (expand-file-name "themes/doom-emacs-bw-light.svg" doom-user-dir))

(set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)

(setq display-line-numbers-type `relative)
(setq-default tab-width 4)

(after! org
(setq org-directory "~/org/"
      org-agenda-files (list org-directory)                  ; Seems like the obvious place.
      org-use-property-inheritance t                         ; It's convenient to have properties inherited.
      org-log-done 'time                                     ; Having the time a item is done sounds convenient.
      org-list-allow-alphabetical t                          ; Have a. A. a) A) list bullets.
      org-catch-invisible-edits 'smart                       ; Try not to accidently do weird stuff in invisible regions.
      org-export-with-sub-superscripts '{}                   ; Don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}.
      org-export-allow-bind-keywords t                       ; Bind keywords can be handy
      org-image-actual-width '(0.9)))                         ; Make the in-buffer display closer to the exported result..

(after! org-babel
(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link"))))

(use-package! org-block-capf
  :after org
  :hook (org-mode . org-block-capf-add-to-completion-at-point-functions))

(after! cape
  (setq cape-dabbrev-min-length 2)
 (add-hook 'completion-at-point-functions #'cape-dabbrev)
 (add-hook 'completion-at-point-functions #'cape-file)
 (add-hook 'completion-at-point-functions #'cape-keyword)
 (add-hook 'completion-at-point-functions #'cape-line)
 (add-hook 'completion-at-point-functions #'cape-history)
 )

(after! org
(setq org-agenda-skip-scheduled-if-done nil
      org-agenda-skip-deadline-if-done nil
        org-agenda-tags-column 0
        org-agenda-block-separator ?─
        org-agenda-time-grid
        '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "◀── now ─────────────────────────────────────────────────"))

(use-package! org-modern
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

(use-package! org-superstar
  :hook (org-mode . (lambda () (org-superstar-mode 1))))

(use-package! org-appear
        :hook (org-mode . org-appear-mode)
        :config
        (setq org-appear-autoemphasis t
                org-appear-autosubmarkers t
                org-appear-autolinks t
                org-appear-autokeywords t
                org-appear-autoentities t
                org-appear-inside-latex t
                org-appear-autosubmarkers t))

(after! org
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
        (?E . 'nerd-icons-blue))))
(add-hook 'org-mode-hook #'+org-pretty-mode)

(after! org
  (setq org-highlight-latex-and-related '(native script entities)))

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(after! org
(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))))

(after! mu4e
    (setq mu4e-org-contacts-file  "~/org/contacts.org")
  (add-to-list 'mu4e-headers-actions
    '("org-contact-add" . mu4e-action-add-org-contact) t)
  (add-to-list 'mu4e-view-actions
    '("org-contact-add" . mu4e-action-add-org-contact) t))

(use-package! org-pandoc-import
  :after org)

;; (use-package! zotxt
;;   :after org)

(use-package! org-chef
  :after org
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

(use-package! citar
  :defer t
  :custom
  (org-cite-global-bibliography '("~/org/Lecture_Notes/MyLibrary.bib"))
  (citar-bibliography org-cite-global-bibliography)
  (citar-symbols
      '(note ,(nerd-icons-octicon "nf-oct-note" :face 'nerd-icons-blue :v-adjust -0.3) . " ")
      '(link ,(nerd-icons-octicon "nf-oct-link" :face 'nerd-icons-orange :v-adjust 0.01) . " "))
  :hook
  (org-mode . citar-capf-setup))

(use-package! citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(use-package! oc-csl
  :after oc
  :config
  (setq org-cite-csl-styles-dir "~/Zotero/styles/"))
(after! oc
 (setq org-cite-export-processors '((t csl))))

(use-package! oc-csl-activate
  :after org
  :config
  (setq org-cite-activate-processor 'csl-activate)
  (setq org-cite-csl-activate-use-document-style t)
  (setq org-cite-csl-activate-use-document-locale t)
  (add-hook! 'org-mode-hook
              (cursor-sensor-mode 1)
              (org-cite-csl-activate-render-all)))

(after! org
  ;; ORG LATEX PREVIEW
  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent"))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1))
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-preview-latex-image-directory "~/.cache/ltximg/")
  )

(after! org
(setq org-format-latex-header "\\documentclass[12pt]
{article}
\\usepackage[usenames]{xcolor}
\\usepackage{booktabs}
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}
% my custom stuff
\\usepackage{xfrac}
\\usepackage{siunitx}
\\usepackage{diffcoeff}
\\usepackage{nicematrix}
\\usepackage[varbb]{newpxmath}
\\DeclareMathOperator{\\Var}{Var}
\\DeclareMathOperator{\\cov}{Cov}
\\DeclareMathOperator{\\E}{\\mathbb{E}}
\\DeclareMathOperator*{\\argmax}{arg\\,max}
\\DeclareMathOperator*{\\argmin}{arg\\,min}
"))

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass[12pt]{article}
\\usepackage[american]{babel}
\\usepackage[margin=1.25in]{geometry}
\\usepackage{parskip}
\\usepackage{booktabs}
\\usepackage{float}
\\usepackage{microtype}
\\usepackage{graphicx}
\\usepackage{mathtools}
\\usepackage{amsthm}
\\usepackage{amssymb}
\\usepackage{bm}
\\usepackage[]{newpxtext}
\\usepackage[]{newpxmath}
\\usepackage{xfrac}
\\usepackage{siunitx}
\\usepackage{caption}
\\captionsetup{labelfont=bf,font={small,singlespacing}}
\\usepackage{subcaption}
\\usepackage{cancel}
\\usepackage{setspace}
\\usepackage{xcolor}
\\usepackage{diffcoeff}
\\usepackage{nicematrix}
\\usepackage{braket}
\\usepackage{enumitem}
\\usepackage{acronym}
\\usepackage{footmisc}
\\usepackage[authoryear,longnamesfirst]{natbib}
\\usepackage{xurl}
\\onehalfspacing{}
\\bibliographystyle{ecta}
\\DeclareMathOperator{\\Var}{Var}
\\DeclareMathOperator{\\Cov}{Cov}
\\DeclareMathOperator{\\E}{\\mathbb{E}}
\\DeclareMathOperator*{\\argmax}{arg\\,max}
\\DeclareMathOperator*{\\argmin}{arg\\,min}
\\newcommand{\\Et}[2]{\\E_{#2} \\left[#1\\right]}
\\newcommand{\\Covt}[3]{\\cov_{#3}\\left(#1, #2\\right)}
\\newcommand{\\Vart}[2]{\\Var_{#2} \\left[#1\\right]}
\\DeclarePairedDelimiter\\abs{\\lvert}{\\rvert}
\\DeclarePairedDelimiter\\norm{\\lVert}{\\rVert}
\\DeclarePairedDelimiterX\\innerp[2]{\\langle}{\\rangle}{#1,#2}
\\theoremstyle{plain}% default
\\newtheorem{thm}{Theorem}
\\newtheorem{lem}[thm]{Lemma}
\\newtheorem{prop}[thm]{Proposition}
\\newtheorem*{cor}{Corollary}
\\theoremstyle{definition}
\\newtheorem{defn}{Definition}
\\newtheorem{exmp}{Example}
\\providecommand*{\\defnautorefname}{Definition}
\\theoremstyle{remark}
\\newtheorem*{rem}{Remark}
\\newtheorem*{note}{Note}
\\newtheorem{case}{Case}
\\renewcommand{\\leq}{\\leqslant}
\\renewcommand{\\geq}{\\geqslant}
\\usepackage{hyperref}
\\usepackage[]{cleveref}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}"))))

(after! org
  (setq org-beamer-frame-level 2))

(after! org
  (setq org-beamer-theme "[progressbar=frametitle, titleformat=smallcaps, numbering=fraction]metropolis"))

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass[c]{beamer}
\\usepackage[american]{babel}
\\usetheme[progressbar=frametitle, titleformat=smallcaps, numbering=fraction]{metropolis}
\\usepackage{parskip}
\\usepackage{booktabs}
\\usepackage{float}
\\usepackage{microtype}
\\usepackage{graphicx}
\\usepackage{mathtools}
\\usepackage{amsthm}
\\usepackage{amssymb}
\\usepackage{bm}
\\usepackage[]{newpxtext}
\\usepackage{newpxmath}
\\usepackage{xfrac}
\\usepackage{siunitx}
\\usepackage{caption}
\\captionsetup{labelfont=bf,font={small,singlespacing}}
\\usepackage{subcaption}
\\usepackage{cancel}
\\usepackage{setspace}
\\usepackage{xcolor}
\\usepackage[ISO]{diffcoeff}
\\usepackage{nicematrix}
\\usepackage{braket}
\\usepackage{enumitem}
\\usepackage{acronym}
\\usepackage{footmisc}
\\usepackage[authoryear,longnamesfirst]{natbib}
\\usepackage{xurl}
\\usepackage{appendixnumberbeamer}
\\usepackage{dirtytalk}
\\DeclareMathOperator{\\Var}{Var}
\\DeclareMathOperator{\\Cov}{Cov}
\\DeclareMathOperator{\\E}{\\mathbb{E}}
\\DeclareMathOperator*{\\argmax}{arg\\,max}
\\DeclareMathOperator*{\\argmin}{arg\\,min}
\\newcommand{\\Et}[2]{\\E_{#2} \\left[#1\\right]}
\\newcommand{\\Covt}[3]{\\cov_{#3}\\left(#1, #2\\right)}
\\newcommand{\\Vart}[2]{\\Var_{#2} \\left[#1\\right]}
\\DeclarePairedDelimiter\\abs{\\lvert}{\\rvert}
\\DeclarePairedDelimiter\\norm{\\lVert}{\\rVert}
\\DeclarePairedDelimiterX\\innerp[2]{\\langle}{\\rangle}{#1,#2}
\\theoremstyle{plain}% default
\\newtheorem{thm}{Theorem}
\\newtheorem{lem}[thm]{Lemma}
\\newtheorem{prop}[thm]{Proposition}
\\newtheorem*{cor}{Corollary}
\\theoremstyle{definition}
\\newtheorem{defn}{Definition}
\\newtheorem{exmp}{Example}
\\providecommand*{\\defnautorefname}{Definition}
\\theoremstyle{remark}
\\newtheorem*{rem}{Remark}
\\newtheorem*{note}{Note}
\\newtheorem{case}{Case}
\\renewcommand{\\leq}{\\leqslant}
\\renewcommand{\\geq}{\\geqslant}
\\definecolor{textcolor}{HTML}{2E3440}
\\definecolor{titlecolor}{HTML}{a3be8c}
\\definecolor{alertcolor}{HTML}{BF616A}
\\definecolor{bgcolor}{HTML}{ECEFF4}
\\definecolor{barcolor}{HTML}{88C0D0}
\\definecolor{bgbarcolor}{HTML}{D8DEE9}
\\setbeamercolor{progress bar}{fg=barcolor,bg=bgbarcolor}
\\setbeamercolor{frametitle}{fg=titlecolor,bg=bgcolor}
\\setbeamercolor{normal text}{fg=textcolor,bg=bgcolor}
\\setbeamercolor{alerted text}{fg=alertcolor,bg=bgcolor}
\\setbeamercolor{example text}{fg=examplecolor}
\\setbeamercovered{dynamic}
\\usecolortheme{rose}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(setq org-latex-pdf-process '("LC_ALL=en_US.UTF-8 latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f"))

(after! org
(setq org-latex-tables-booktabs t
      org-latex-hyperref-template "\\providecolor{url}{HTML}{81a1c1}
\\providecolor{link}{HTML}{d08770}
\\providecolor{cite}{HTML}{d08770}
\\hypersetup{
pdfauthor={%a},
pdftitle={%t},
pdfkeywords={%k},
pdfsubject={%d},
pdfcreator={%c},
pdflang={%L},
breaklinks=true,
colorlinks=true,
linkcolor=link,
urlcolor=url,
citecolor=cite
}
"
      org-latex-reference-command "\\cref{%s}"))

;; Use pdf-tools to open PDF files
(after! auctex
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package! engrave-faces-latex
  :after ox-latex)
(setq org-latex-listings 'engraved)
(setq org-latex-engraved-theme 'doom-nord)

(use-package! doct
  :after org)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO [#B] %?\n:Created: %T\n")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
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
 ("wp" "Phone Call" entry (file+datetree "~/org/work.org") "* Phone call about %?\nSCHEDULED:%t\nDEADLINE: %^T\n\n%i" :clock-in t)
 ("wm" "Meeting"    entry (file+datetree "~/org/work.org") "* Meeting about %?\nSCHEDULED:%t\nDEADLINE: %^T\n\n%i"    :clock-in t)
 ("m" "Email Workflow")
    ("mw" "Write" entry (file+olp "~/org/mail.org" "New")
          "* TODO Email %?\nSCHEDULED:%t\nDEADLINE: %^T\n\n%i" :immediate-finish t)
    ("mf" "Follow Up" entry (file+olp "~/org/mail.org" "Follow Up")
          "* TODO Follow up with %:fromname on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
    ("mr" "Read Later" entry (file+olp "~/org/mail.org" "Read Later")
          "* TODO Read %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t)
        ))

(use-package! jinx
  :defer t
  :init
  (global-jinx-mode)
  :config
  ;; Extra face(s) to ignore
  (push 'org-inline-src-block
        (alist-get 'org-mode jinx-exclude-faces))
  ;; Take over the relevant bindings.
  (after! evil-commands
    (global-set-key [remap ispell-word] #'jinx-correct))
  (after! evil-commands
    (global-set-key [remap evil-next-flyspell-error] #'jinx-next)
    (global-set-key [remap evil-prev-flyspell-error] #'jinx-previous)))

(defcustom lsp-ltex-active-modes
  '(text-mode
    bibtex-mode context-mode
    latex-mode LaTeX-mode ;; AUCTeX 14+ has renamed latex-mode to LaTeX-mode
    markdown-mode org-mode
    rst-mode
    org-msg-edit-mode
    mu4e-compose-mode)
  "List of major mode that work with LTEX Language Server."
  :type 'list
  :group 'lsp-ltex)

(use-package! lsp-ltex
  :defer t
  :init)
(after! lsp-ltex
  (appendq! lsp-language-id-configuration
            '((mu4e-compose-mode . "plaintext"))))
(use-package orderless
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :config
    (setq lsp-warn-no-matched-clients 'nil)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))
;; (use-package! eglot-ltex                ;
;;   :init
;;   (setq eglot-ltex-server-path "/opt/homebrew/"
;;         eglot-ltex-communication-channel 'tcp))         ; 'stdio or 'tcp

;; (after! eglot
;;   (add-to-list 'eglot-server-programs
;;               `((latex-mode :language-id "latex")
;;                 . ,(eglot-alternatives '(("texlab")
;;                                          ("ltex-ls" "--server-type" "TcpSocket" "--port" :autoport)))))) ;

(use-package! vlf-setup
  :defer t)

(use-package! csv-mode
  :defer t
  :hook ((csv-mode . csv-align-mode)
         (csv-mode . csv-header-line)
         )
  )

;; (add-hook 'pdf-tools-enabled-hook 'pdf-view-midnight-minor-mode)

(defun switch-left-and-right-option-keys ()
  "Switch left and right option keys.
     On some external keyboards the left and right option keys are swapped,
     this command switches the keys so that they work as expected."
  (interactive)
  (let ((current-left  mac-option-modifier)
        (current-right mac-right-option-modifier))
    (setq mac-option-modifier       current-right
          mac-right-option-modifier current-left)))

;; mac switch meta key
(defun mac-switch-meta nil
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
	(setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'hyper)
	)
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta)
      )
    )
  )

;; (add-hook! 'elfeed-search-mode-hook #'elfeed-update) ;
;; (after! elfeed
;;   (setq elfeed-goodies/entry-pane-position 'bottom)
;;   (setq rmh-elfeed-org-files '("~/org/elfeed.org")))

;; (after! elfeed
;;   (setq elfeed-search-filter "@1-week-ago +unread"
;;         elfeed-search-print-entry-function '+rss/elfeed-search-print-entry
;;         elfeed-search-title-min-width 80
;;         elfeed-show-entry-switch #'pop-to-buffer
;;         elfeed-show-entry-delete #'elfeed-kill-buffer
;;         elfeed-show-refresh-function #'+rss/elfeed-show-refresh--better-style
;;         shr-max-image-proportion 0.6)

;;   (add-hook! 'elfeed-show-mode-hook (hide-mode-line-mode 1))
;;   (add-hook! 'elfeed-search-update-hook #'hide-mode-line-mode)

;;   (defface elfeed-show-title-face '((t (:weight ultrabold :slant italic :height 1.5)))
;;     "title face in elfeed show buffer"
;;     :group 'elfeed)
;;   (defface elfeed-show-author-face `((t (:weight light)))
;;     "title face in elfeed show buffer"
;;     :group 'elfeed)
;;   (set-face-attribute 'elfeed-search-title-face nil
;;                       :foreground 'nil
;;                       :weight 'light)

;;   (defadvice! +rss-elfeed-wrap-h-nicer ()
;;     "Enhances an elfeed entry's readability by wrapping it to a width of
;; `fill-column' and centering it with `visual-fill-column-mode'."
;;     :override #'+rss-elfeed-wrap-h
;;     (setq-local truncate-lines nil
;;                 shr-width 140
;;                 visual-fill-column-center-text t
;;                 default-text-properties '(line-height 1.2))
;;     (let ((inhibit-read-only t)
;;           (inhibit-modification-hooks t))
;;        (setq-local shr-current-font '(:family "Lato" :height 1.2))
;;       (set-buffer-modified-p nil)))

;;   (defun +rss/elfeed-search-print-entry (entry)
;;     "Print ENTRY to the buffer."
;;     (let* ((elfeed-goodies/tag-column-width 40)
;;            (elfeed-goodies/feed-source-column-width 30)
;;            (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
;;            (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
;;            (feed (elfeed-entry-feed entry))
;;            (feed-title
;;             (when feed
;;               (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
;;            (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
;;            (tags-str (concat (mapconcat 'identity tags ",")))
;;            (title-width (- (window-width) elfeed-goodies/feed-source-column-width
;;                            elfeed-goodies/tag-column-width 4))

;;            (tag-column (elfeed-format-column
;;                         tags-str (elfeed-clamp (length tags-str)
;;                                                elfeed-goodies/tag-column-width
;;                                                elfeed-goodies/tag-column-width)
;;                         :left))
;;            (feed-column (elfeed-format-column
;;                          feed-title (elfeed-clamp elfeed-goodies/feed-source-column-width
;;                                                   elfeed-goodies/feed-source-column-width
;;                                                   elfeed-goodies/feed-source-column-width)
;;                          :left)))

;;       (insert (propertize feed-column 'face 'elfeed-search-feed-face) " ")
;;       (insert (propertize tag-column 'face 'elfeed-search-tag-face) " ")
;;       (insert (propertize title 'face title-faces 'kbd-help title))
;;       (setq-local line-spacing 0.2)))

;;   (defun +rss/elfeed-show-refresh--better-style ()
;;     "Update the buffer to match the selected entry, using a mail-style."
;;     (interactive)
;;     (let* ((inhibit-read-only t)
;;            (title (elfeed-entry-title elfeed-show-entry))
;;            (date (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
;;            (author (elfeed-meta elfeed-show-entry :author))
;;            (link (elfeed-entry-link elfeed-show-entry))
;;            (tags (elfeed-entry-tags elfeed-show-entry))
;;            (tagsstr (mapconcat #'symbol-name tags ", "))
;;            (nicedate (format-time-string "%a, %e %b %Y %T %Z" date))
;;            (content (elfeed-deref (elfeed-entry-content elfeed-show-entry)))
;;            (type (elfeed-entry-content-type elfeed-show-entry))
;;            (feed (elfeed-entry-feed elfeed-show-entry))
;;            (feed-title (elfeed-feed-title feed))
;;            (base (and feed (elfeed-compute-base (elfeed-feed-url feed)))))
;;       (erase-buffer)
;;       (insert "\n")
;;       (insert (format "%s\n\n" (propertize title 'face 'elfeed-show-title-face)))
;;       (insert (format "%s\t" (propertize feed-title 'face 'elfeed-search-feed-face)))
;;       (when (and author elfeed-show-entry-author)
;;         (insert (format "%s\n" (propertize author 'face 'elfeed-show-author-face))))
;;       (insert (format "%s\n\n" (propertize nicedate 'face 'elfeed-log-date-face)))
;;       (when tags
;;         (insert (format "%s\n"
;;                         (propertize tagsstr 'face 'elfeed-search-tag-face))))
;;       ;; (insert (propertize "Link: " 'face 'message-header-name))
;;       ;; (elfeed-insert-link link link)
;;       ;; (insert "\n")
;;       (cl-loop for enclosure in (elfeed-entry-enclosures elfeed-show-entry)
;;                do (insert (propertize "Enclosure: " 'face 'message-header-name))
;;                do (elfeed-insert-link (car enclosure))
;;                do (insert "\n"))
;;       (insert "\n")
;;       (if content
;;           (if (eq type 'html)
;;               (elfeed-insert-html content base)
;;             (insert content))
;;         (insert (propertize "(empty)\n" 'face 'italic)))
;;       (goto-char (point-min))))

;;   )

;; add to $DOOMDIR/config.el
(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail)
  ;; how often to call it in seconds:
  (setq   mu4e-sent-messages-behavior 'sent ;; Save sent messages
          mu4e-headers-auto-update t                ; avoid to type `g' to update
          mml-secure-openpgp-signers '("6A5C039B63B86AC6C5109955B57BA04FBD759C7F" "D1D9947126EE64AC7ED3950196F352393B5B3C2E")
          mml-secure-openpgp-sign-with-sender t
          mu4e-use-fancy-chars t                   ; allow fancy icons for mail threads
          mu4e-change-filenames-when-moving t
          mu4e-index-lazy-check nil
          mu4e-search-results-limit 300
          mu4e-context-policy 'pick-first ;; Always ask which context to use when composing a new mail
          mu4e-compose-context-policy 'ask ;; Always ask which context to use when composing a new mail
          mu4e-update-interval 60
          mu4e-mu-allow-temp-file t
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
          mu4e-contexts '()
          )
)
  (set-email-account! "gmail"
                      '((mu4e-sent-folder       . "/leoaparisi@gmail.com/[Gmail]/Sent Mail")
                        (mu4e-drafts-folder     . "/leoaparisi@gmail.com/[Gmail]/Drafts")
                        (mu4e-trash-folder      . "/leoaparisi@gmail.com/[Gmail]/Trash")
                        (mu4e-refile-folder     . "/leoaparisi@gmail.com/Archives")
                        (user-mail-address . "leoaparisi@gmail.com")
                        (smtpmail-smtp-user     . "leoaparisi@gmail.com"))
                      t)
  (set-email-account! "university"
                      '((mu4e-sent-folder       . "/laparisidelannoy@uchicago.edu/Sent")
                        (mu4e-drafts-folder     . "/laparisidelannoy@uchicago.edu/Drafts")
                        (user-mail-address . "laparisidelannoy@uchicago.edu")
                        (mu4e-trash-folder      . "/laparisidelannoy@uchicago.edu/Trash")
                        (mu4e-refile-folder     . "/laparisidelannoy@uchicago.edu/Archive")
                        (smtpmail-smtp-user     . "laparisidelannoy@uchicago.edu"))
                      t)
(add-hook! 'mu4e-compose-mode-hook#'org-msg-edit-mode)

;;  (setq mail-user-agent 'notmuch-user-agent)
;; (after! notmuch
;;   (setq sendmail-program (executable-find "msmtp")
;;         send-mail-function #'smtpmail-send-it
;;         message-sendmail-f-is-evil t
;;         message-sendmail-extra-arguments '("--read-envelope-from")
;;         message-send-mail-function #'message-send-mail-with-sendmail
;;         mail-specify-envelope-from t
;;         message-sendmail-envelope-from 'header
;;         mail-envelope-from 'header
;;         +notmuch-sync-backend 'mbsync
;;    ))
(use-package! org-msg
  :after org
  :config
   ;; :hook (notmuch-hello-mode . org-msg-mode)
   ;; :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil tex:dvipng"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-name-limit 3
        org-msg-default-alternatives '((new . (html))
					  (reply-to-html . (html)))
        org-msg-convert-citation t
        org-msg-signature "
#+begin_signature
Leo Aparisi de Lannoy
#+end_signature"))

(after! auctex
(setq +latex-viewers '(pdf-tools))
(setq TeX-command-default "laTeXMk")
(defun compile-save()
  "Test of save hook"
  (when (eq major-mode 'LaTeX-mode)
    (+latex/compile)))
(add-hook 'after-save-hook #'compile-save)
(setq TeX-save-query nil
      TeX-show-compilation nil
      TeX-engine "luatex"
      TeX-command-extra-options "-lualatex -shell-escape"))

;; (setq flycheck-eglot-exclusive nil)
(after! flycheck
(map! :map evil-normal-state-map
      "SPC c b" #'consult-flycheck)
(setq flycheck-checker-error-threshold 5000)
(flycheck-define-checker vale
  "A checker for prose"
  :command ("vale" "--output" "line"
            source)
  :standard-input nil
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
  :modes (markdown-mode org-mode text-mode)
  )
(add-to-list 'flycheck-checkers 'vale 'append)
(defconst flycheck-org-lint-form
  (flycheck-prepare-emacs-lisp-form
   (require 'org)
   (require 'org-attach)
   (let ((source (car command-line-args-left))
   (process-default-directory default-directory))
   (with-temp-buffer
   (insert-file-contents source 'visit)
   (setq buffer-file-name source)
   (setq default-directory process-default-directory)
   (delay-mode-hooks (org-mode))
   (setq delayed-mode-hooks nil)
   (dolist (err (org-lint))
   (let ((inf (cl-second err)))
   (princ (elt inf 0))
   (princ ": ")
   (princ (elt inf 2))
   (terpri)))))))

(defconst flycheck-org-lint-variables
    '(org-directory
      org-id-locations
      org-id-locations-file
      org-attach-id-dir
      org-attach-use-inheritance
      org-attach-id-to-path-function-list)
    "Variables inherited by the org-lint subprocess.")

(defun flycheck-org-lint-variables-form ()
  "Make org-lint availables available."
    (require 'org-attach)
    `(progn
       ,@(seq-map (lambda (opt) `(setq-default ,opt ',(symbol-value opt)))
                  (seq-filter #'boundp flycheck-org-lint-variables))))

(flycheck-define-checker org-lint
  "Org buffer checker using `org-lint'.

See URL `https://orgmode.org/'."
  :command ("emacs" (eval flycheck-emacs-args)
              "--eval" (eval flycheck-org-lint-form)
              "--" source)
  :error-patterns
  ((error line-start line ": " (message) line-end))
  :modes (org-mode)
  :next-checkers (vale))

(add-to-list 'flycheck-checkers 'org-lint))
;;; flycheck-org-lint.el ends here

(after! tramp
 (setenv "SHELL" "/bin/bash")
 (setq tramp-shell-prompt-pattern "\\(?:^\\|\n\\|\x0d\\)[^]#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 
 (setq vc-ignore-dir-regexp
               (format "\\(%s\\)\\|\\(%s\\)"
                       vc-ignore-dir-regexp
                       tramp-file-name-regexp))

(use-package! browser-hist
 :config
 (setq browser-hist-default-browser 'brave)
 :commands (browser-hist-search))

(use-package! youtube-sub-extractor
:commands (youtube-sub-extractor-extract-subs)
:config
(map! :map youtube-sub-extractor-subtitles-mode-map
  :desc "copy timestamp URL" :n "RET" #'youtube-sub-extractor-copy-ts-link
  :desc "browse at timestamp" :n "C-c C-o" #'youtube-sub-extractor-browse-ts-link))

(setq youtube-sub-extractor-timestamps 'left-margin)

(use-package! ultra-scroll-mac
 :if (eq window-system 'mac)
 ;:load-path "~/code/emacs/ultra-scroll-mac" ; if you git clone'd instead of package-vc-install
 :init
 (setq scroll-conservatively 101 ; important!
       scroll-margin 0)
 :config
 (ultra-scroll-mac-mode 1))

(use-package indent-bars
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

(use-package! treesit)
(use-package! treesit-auto
        :config
        (setq treesit-auto-install 'prompt
                treesit-auto-mode t))
(use-package! evil-textobj-tree-sitter
  :defer t
  :init (after! treesit )
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

(after! projectile
  (setq projectile-indexing-method 'alien))

(use-package! jupyter-client
  :defer t)

;;  (use-package! eat
;;    :defer t
;;    :config
;;    (setq eat-very-visible-cursor-type '(t nil hollow)
;;          eat-enable-auto-line-mode t)
;;    )
;; (after! evil-commands
;;     (global-set-key [remap +vterm/toggle] #'eat-other-window)
;;     (global-set-key [remap +vterm/here] #'eat)
;;     )
