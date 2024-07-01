;; brew tap railwaycat/emacsmacport
;; brew install emacs-mac --with-mac-metal --with-natural-title-bar --with-native-compilation --with-xwidget

(setq user-full-name "Leo Aparisi de Lannoy"
      user-mail-address "leoaparisi@gmail.com")

(setq dired-async-mode 1)

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t)                      ; take new window space from all other windows (not just current)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t)                         ; Nobody likes to loose work, I certainly don't

(setq browse-url-chrome-program "brave")

(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-variable-pitch-font (font-spec :family "Lato")
      doom-unicode-font (font-spec :family "Iosevka")
      doom-big-font (font-spec :family "Iosevka" :size 24)
      doom-serif-font (font-spec :family "Iosevka Aile" :weight 'light))

(setq doom-theme 'doom-nord)
;; (load-theme 'catppuccin t t)
;; (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha

;; (setq fancy-splash-image (expand-file-name "themes/doom-emacs-bw-light.svg" doom-user-dir))

(set-file-template! "\\.tex$" :trigger "__" :mode 'latex-mode)
(set-file-template! "\\.org$" :trigger "__" :mode 'org-mode)

(setq display-line-numbers-type `relative)
(setq-default tab-width 4)

(setq org-directory "~/org/"
      org-agenda-files (list org-directory)                  ; Seems like the obvious place.
      org-use-property-inheritance t                         ; It's convenient to have properties inherited.
      org-log-done 'time                                     ; Having the time a item is done sounds convenient.
      org-list-allow-alphabetical t                          ; Have a. A. a) A) list bullets.
      org-catch-invisible-edits 'smart                       ; Try not to accidently do weird stuff in invisible regions.
      org-export-with-sub-superscripts '{}                   ; Don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}.
      org-export-allow-bind-keywords t                       ; Bind keywords can be handy
      org-image-actual-width '(0.9))                         ; Make the in-buffer display closer to the exported result..#+end_src

(setq org-babel-default-header-args
      '((:session . "none")
        (:results . "replace")
        (:exports . "code")
        (:cache . "no")
        (:noweb . "no")
        (:hlines . "no")
        (:tangle . "no")
        (:comments . "link")))

(use-package! org-block-capf
  :after org
  :hook (org-mode . org-block-capf-add-to-completion-at-point-functions))

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

(use-package! org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((43 . "➤")
                          (45 . "–")
                          (42 . "•"))
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("PROJ" :inverse-video t :inherit +org-todo-project)
          ("STRT" :inverse-video t :inherit +org-todo-active)
          ("[-]"  :inverse-video t :inherit +org-todo-active)
          ("HOLD" :inverse-video t :inherit +org-todo-onhold)
          ("WAIT" :inverse-video t :inherit +org-todo-onhold)
          ("[?]"  :inverse-video t :inherit +org-todo-onhold)
          ("KILL" :inverse-video t :inherit +org-todo-cancel)
          ("NO"   :inverse-video t :inherit +org-todo-cancel))
        org-modern-footnote
        (cons nil (cadr org-script-display))
        org-modern-block-fringe nil
        org-modern-block-name
        '((t . t)
          ("src" "»" "«")
          ("example" "»–" "–«")
          ("quote" "❝" "❞")
          ("export" "⏩" "⏪"))
        org-modern-progress nil
        org-modern-priority nil
        org-modern-horizontal-rule (make-string 36 ?─)
        org-modern-keyword
        '((t . t)
          ("title" . "𝙏")
          ("subtitle" . "𝙩")
          ("author" . "𝘼")
          ("email" . #("" 0 1 (display (raise -0.14))))
          ("date" . "𝘿")
          ("property" . "☸")
          ("options" . "⌥")
          ("startup" . "⏻")
          ("macro" . "𝓜")
          ("bind" . #("" 0 1 (display (raise -0.1))))
          ("bibliography" . "")
          ("print_bibliography" . #("" 0 1 (display (raise -0.1))))
          ("cite_export" . "⮭")
          ("print_glossary" . #("ᴬᶻ" 0 1 (display (raise -0.1))))
          ("glossary_sources" . #("" 0 1 (display (raise -0.14))))
          ("include" . "⇤")
          ("setupfile" . "⇚")
          ("html_head" . "🅷")
          ("html" . "🅗")
          ("latex_class" . "🄻")
          ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
          ("latex_header" . "🅻")
          ("latex_header_extra" . "🅻⁺")
          ("latex" . "🅛")
          ("beamer_theme" . "🄱")
          ("beamer_color_theme" . #("🄱" 1 2 (display (raise -0.12))))
          ("beamer_font_theme" . "🄱𝐀")
          ("beamer_header" . "🅱")
          ("beamer" . "🅑")
          ("attr_latex" . "🄛")
          ("attr_html" . "🄗")
          ("attr_org" . "⒪")
          ("call" . #("" 0 1 (display (raise -0.15))))
          ("name" . "⁍")
          ("header" . "›")
          ("caption" . "☰")
          ("results" . "🠶")))
  (custom-set-faces! '(org-modern-statistics :inherit org-checkbox-statistics-todo)))

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(setq org-src-fontify-natively t
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t
      org-startup-with-inline-images t
      org-startup-indented t
      ;; Org styling, hide markup etc.
      org-pretty-entities t
      )

(setq org-ellipsis " ▾ "
      org-hide-leading-stars t
      org-priority-highest ?A
      org-priority-lowest ?E
      org-priority-faces
      '((?A . 'nerd-icons-red)
        (?B . 'nerd-icons-orange)
        (?C . 'nerd-icons-yellow)
        (?D . 'nerd-icons-green)
        (?E . 'nerd-icons-blue)))

(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))
(custom-set-faces!
  '(org-document-title :height 1.2))

(setq org-highlight-latex-and-related '(native script entities))

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

(after! org-agenda
  (setq org-agenda-deadline-faces
      '((1.001 . error)
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . org-upcoming-distant-deadline))))

;; (use-package! org-roam
;;   :after org
;;   :config
;;   (setq                   org-enable-roam-support t
;;                           org-roam-directory (concat org-directory "/Roam")
;;                           org-roam-v2-ack t))

;; (setq org-roam-dailies-directory "daily/")

;; (setq org-roam-dailies-capture-templates
;;       '(("d" "default" entry
;;          "* %?"
;;          :target (file+head "%<%Y-%m-%d>.org"
;;                             "#+title: %<%Y-%m-%d>\n"))))

;; (defadvice! doom-modeline--buffer-file-name-roam-aware-a (orig-fun)
;;   :around #'doom-modeline-buffer-file-name ; takes no args
;;   (if (s-contains-p org-roam-directory (or buffer-file-name ""))
;;       (replace-regexp-in-string
;;        "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
;;        "🢔(\\1-\\2-\\3) "
;;        (subst-char-in-string ?_ ?  buffer-file-name))
;;     (funcall orig-fun)))
;; (use-package! websocket
;;   :after org-roam)
;; (use-package! org-roam-ui
;;   :after org-roam
;;   :commands org-roam-ui-open
;;   :hook (org-roam . org-roam-ui-mode)
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t)
;;   (require 'org-roam) ; in case autoloaded
;;   (defun org-roam-ui-open ()
;;     "Ensure the server is active, then open the roam graph."
;;     (interactive)
;;     (unless org-roam-ui-mode (org-roam-ui-mode 1))
;;     (browse-url--browser (format "http://localhost:%d" org-roam-ui-port))))

(use-package! org-pandoc-import
  :after org)

;; (map! :map org-mode-map

;;       :localleader
;;       :desc "View exported file" "v" #'org-view-output-file)

;; (defun org-view-output-file (&optional org-file-path)
;;   "Visit buffer open on the first output file (if any) found, using `org-view-output-file-extensions'"
;;   (interactive)
;;   (let* ((org-file-path (or org-file-path (buffer-file-name) ""))
;;          (dir (file-name-directory org-file-path))
;;          (basename (file-name-base org-file-path))
;;          (output-file nil))
;;     (dolist (ext org-view-output-file-extensions)
;;       (unless output-file
;;         (when (file-exists-p
;;                (concat dir basename "." ext))
;;           (setq output-file (concat dir basename "." ext)))))
;;     (if output-file
;;         (if (member (file-name-extension output-file) org-view-external-file-extensions)
;;             (browse-url-xdg-open output-file)
;;           (pop-to-buffer (or (find-buffer-visiting output-file)
;;                              (find-file-noselect output-file))))
;;       (message "No exported file found"))))

;; (defvar org-view-output-file-extensions '("pdf" "md" "rst" "txt" "tex" "html")
;;   "Search for output files with these extensions, in order, viewing the first that matches")
;; (defvar org-view-external-file-extensions '("html")
;;   "File formats that should be opened externally.")

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

;; (use-package! citar-org-roam
;;   :after citar org-roam
;;   :config (citar-org-roam-mode))
;; (setq org-roam-capture-templates
;;       '(("d" "default" plain
;;          "%?"
;;          :target
;;          (file+head
;;           "%<%Y%m%d%H%M%S>-${slug}.org"
;;           "#+title: ${title}\n")
;;          :unnarrowed t)
;;         ("n" "literature note" plain
;;          "%?"
;;          :target
;;          (file+head
;;           "%(expand-file-name \"literature\" org-roam-directory)/${citekey}.org"
;;           "#+title: ${citekey}. ${title}.\n#+created: %U\n#+last_modified: %U\n\n")
;;          :unnarrowed t)))
;; (setq citar-org-roam-capture-template-key "n")

(after! org
  ;; ORG LATEX PREVIEW
  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent"))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1))
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-preview-latex-image-directory "~/.cache/ltximg/")
  )

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
")

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

(setq org-beamer-theme "[progressbar=frametitle, titleformat=smallcaps, numbering=fraction]metropolis")

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
      org-latex-reference-command "\\cref{%s}")



;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(use-package! engrave-faces-latex
  :after ox-latex)
(setq org-latex-listings 'engraved)
(setq org-latex-engraved-theme 'doom-nord)

(use-package! doct
  :after org)

;; (setq corfu-popupinfo-delay 0)

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

(add-hook 'LaTeX-mode-hook #'lsp-deferred)

;; (map! :after lsp-mode
;;        :map evil-normal-state-map
;;        "K" #'lsp-ui-doc-show)

;; (setq native-comp-deferred-compilation-deny-list '("lsp-bridge"))
;; (use-package! lsp-bridge
;;   :config
;;   (setq lsp-bridge-enable-log nil)
;;   (global-lsp-bridge-mode))

;; (after! lsp-mode
;;   (setq lsp-tex-server 'digestif))

(use-package! lsp-ltex
  :defer t
  :init)
;; (use-package! eglot-ltex                ;
;;   :init
;;   (setq eglot-ltex-server-path "/opt/homebrew/"
;;         eglot-ltex-communication-channel 'tcp))         ; 'stdio or 'tcp

;; (after! eglot
;;   (add-to-list 'eglot-server-programs
;;               `((latex-mode :language-id "latex")
;;                 . ,(eglot-alternatives '(("texlab")
;;                                          ("ltex-ls" "--server-type" "TcpSocket" "--port" :autoport)))))) ;

;; (use-package! vlf-setup
;;   :defer-incrementally t)

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

;; (after! centaur-tabs
;;   (centaur-tabs-mode -1)
;;   (setq centaur-tabs-height 36
;;         centaur-tabs-set-icons t
;;         centaur-tabs-modified-marker "o"
;;         centaur-tabs-close-button "×"
;;         centaur-tabs-set-bar 'above
;;         centaur-tabs-gray-out-icons 'buffer)
;;   (centaur-tabs-change-fonts "P22 Underground Book" 160))
;; (setq x-underline-at-descent-line t)

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
          mu4e-context-policy 'pick-first ;; Always ask which context to use when composing a new mail
          mu4e-compose-context-policy 'ask ;; Always ask which context to use when composing a new mail
          mu4e-update-interval 60
          mu4e-mu-allow-temp-file t
          mu4e-attachment-dir "~/Downloads")
  (set-email-account! "gmail"
                      '((mu4e-sent-folder       . "/leoaparisi@gmail.com/[Gmail]/Sent Mail")
                        (mu4e-drafts-folder     . "/leoaparisi@gmail.com/[Gmail]/Drafts")
                        (mu4e-trash-folder      . "/leoaparisi@gmail.com/[Gmail]/Trash")
                        (mu4e-refile-folder     . "/leoaparisi@gmail.com/Archives")
                        (user-mail-address . "leoaparisi@gmail.com")
                        (smtpmail-smtp-user     . "leoaparisi@gmail.com")
                        (mu4e-compose-signature . "---\nLeo Aparisi de Lannoy"))
                      t)
  (set-email-account! "U Chicago"
                      '((mu4e-sent-folder       . "/laparisidelannoy@uchicago.edu/Sent")
                        (mu4e-drafts-folder     . "/laparisidelannoy@uchicago.edu/Drafts")
                        (user-mail-address . "laparisidelannoy@uchicago.edu")
                        (mu4e-trash-folder      . "/laparisidelannoy@uchicago.edu/Trash")
                        (mu4e-refile-folder     . "/laparisidelannoy@uchicago.edu/Archive")
                        (smtpmail-smtp-user     . "laparisidelannoy@uchicago.edu")
                        (mu4e-compose-signature . "---\nLeo Aparisi de Lannoy"))
                      t)
  ;; Add a unified inbox shortcut
  (add-to-list
   'mu4e-bookmarks
   '(:name "Unified inbox" :query "maildir:/.*inbox/" :key ?i) t)
  (add-to-list
   'mu4e-bookmarks
   '(:name "Sent" :query "maildir:/.*Sent/" :key ?s) t)
  (add-to-list
   'mu4e-bookmarks
   '(:name "Drafts" :query "maildir:/.*Drafts/" :key ?d) t)
  (add-to-list
   'mu4e-bookmarks
   '(:name "Spam" :query "maildir:/.*Spam/ or maildir:/.*Junk/" :key ?S) t)
  (add-to-list
   'mu4e-bookmarks
   '(:name "Trash" :query "maildir:/.*Trash/" :key ?T) t)
  )

(after! org-msg
   (setq	org-msg-convert-citation t
            org-msg-signature "
#+begin_signature
Leo Aparisi de Lannoy
#+end_signature"))

(after! latex
(setq +latex-viewers '(pdf-tools))
(defun compile-save()
  "Test of save hook"
  (when (eq major-mode 'LaTeX-mode)
    (+latex/compile)))
(add-hook 'after-save-hook #'compile-save)
(setq TeX-save-query nil
      TeX-show-compilation nil
      TeX-command-extra-options "-shell-escape")
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t)))

;; (setq flycheck-eglot-exclusive nil)
(map! :map evil-normal-state-map
      "SPC c b" #'consult-flycheck)

(after! tramp
 (setenv "SHELL" "/bin/bash")
 (setq tramp-shell-prompt-pattern "\\(?:^\\|\n\\|\x0d\\)[^]#$%>\n]*#?[]#$%>] *\\(\e\\[[0-9;]*[a-zA-Z] *\\)*")) ;; default + 
 (setq vc-ignore-dir-regexp
               (format "\\(%s\\)\\|\\(%s\\)"
                       vc-ignore-dir-regexp
                       tramp-file-name-regexp))
