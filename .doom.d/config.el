;; brew tap railwaycat/emacsmacport
;; brew install emacs-mac --with-mac-metal --with-natural-title-bar --with-native-compilation --with-xwidget

(setq user-full-name "Leo Aparisi de Lannoy"
      user-mail-address "leoaparisi@gmail.com")

(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"
      scroll-margin 1)                            ; It's nice to maintain a little margin

(global-subword-mode 1)                           ; Iterate through CamelCase words

(setq browse-url-chrome-program "brave")

(setq which-key-idle-delay 0.5 ;; Default is 1.0
      which-key-idle-secondary-delay 0.05) ;; Default is nil
(setq which-key-allow-multiple-replacements t)

(after! which-key
  (pushnew! which-key-replacement-alist
            '((""       . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "🅔·\\1"))
            '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)")       . (nil . "Ⓔ·\\1"))))

(setq doom-font (font-spec :family "Iosevka" :size 14)
      doom-variable-pitch-font (font-spec :family "Lato")
      doom-unicode-font (font-spec :family "JuliaMono")
      doom-big-font (font-spec :family "Iosevka" :size 24)
      doom-serif-font (font-spec :family "Iosevka Aile" :weight 'light))

(load-theme 'catppuccin t t)
(setq doom-theme 'catppuccin)
 (setq catppuccin-flavor 'mocha) ;; or 'latte, 'macchiato, or 'mocha

(setq fancy-splash-image (expand-file-name "themes/doom-emacs-bw-light.svg" doom-user-dir))

(use-package! theme-magic
  :commands theme-magic-from-emacs
  :config
  (defadvice! theme-magic--auto-extract-16-doom-colors ()
    :override #'theme-magic--auto-extract-16-colors
    (list
     (face-attribute 'default :background)
     (doom-color 'error)
     (doom-color 'success)
     (doom-color 'type)
     (doom-color 'keywords)
     (doom-color 'constants)
     (doom-color 'functions)
     (face-attribute 'default :foreground)
     (face-attribute 'shadow :foreground)
     (doom-blend 'base8 'error 0.1)
     (doom-blend 'base8 'success 0.1)
     (doom-blend 'base8 'type 0.1)
     (doom-blend 'base8 'keywords 0.1)
     (doom-blend 'base8 'constants 0.1)
     (doom-blend 'base8 'functions 0.1)
     (face-attribute 'default :foreground))))

(after! info-colors
  :commands (info-colors-fontify-node))
(add-hook! 'Info-selection-hook 'info-colors-fontify-node)

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
(global-org-modern-mode)

(after! spell-fu
  (cl-pushnew 'org-modern-tag (alist-get 'org-mode +spell-excluded-faces-alist)))

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

(require 'org-src)
(add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))

;; (add-hook 'org-mode-hook #'org-latex-preview-auto-mode)
(add-hook 'org-mode-hook 'org-fragtog-mode)

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

(setq ob-async-no-async-languages-alist '("jupyter-python" "jupyter-julia"))

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
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

(use-package! citar
  :no-require
  :custom
  (org-cite-global-bibliography '("~/org/Lecture_Notes/MyLibrary.bib"))
  (citar-bibliography org-cite-global-bibliography)
  (citar-symbols
      `(note ,(nerd-icons-octicon "nf-oct-note" :face 'nerd-icons-blue :v-adjust -0.3) . " ")
      (link ,(nerd-icons-octicon "nf-oct-link" :face 'nerd-icons-orange :v-adjust 0.01) . " ")))

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
\\usepackage{graphicx}
\\usepackage{mathtools}
\\usepackage{wrapfig}
\\usepackage{amsthm}
\\usepackage{amssymb}
\\usepackage{newpxtext}
\\usepackage[varbb]{newpxmath}
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
\\usepackage{enumitem}
\\usepackage{acronym}
\\usepackage{xurl}
\\onehalfspacing{}
\\DeclareMathOperator{\\Var}{Var}
\\DeclareMathOperator{\\cov}{Cov}
\\DeclareMathOperator{\\E}{\\mathbb{E}}
\\DeclareMathOperator*{\\argmax}{arg\\,max}
\\DeclareMathOperator*{\\argmin}{arg\\,min}
\\newcommand{\\Et}[2]{\\E_{#2} \\left[#1\\right]}
\\newcommand{\\Covt}[3]{\\cov_{#3}\\left(#1, #2\\right)}
\\newcommand{\\Vart}[2]{\\Var_{#2} \\left[#1\\right]}
\\DeclarePairedDelimiter\\abs{\\lvert}{\\rvert}
\\DeclarePairedDelimiter\\norm{\\lVert}{\\rVert}
\\DeclarePairedDelimiterX\\innerp[2]{\\langle}{\\rangle}{#1,#2}
\\DeclarePairedDelimiterX\\braket[3]{\\langle}{\\rangle}%
{#1\\,\\delimsize\\vert\\,\\mathopen{}#2\\,\\delimsize\\vert\\,\\mathopen{}#3}
\\providecommand\\given{}
\\DeclarePairedDelimiterXPP\\Prob[1]{\\mathbb{P}} (){}{
\\renewcommand\\given{\\nonscript\\:\\delimsize\\vert\\nonscript\\:\\mathopen{}}
#1}
\\DeclarePairedDelimiterXPP\\condE[1]{\\E} (){}{
\\renewcommand\\given{\\nonscript\\:\\delimsize\\vert\\nonscript\\:\\mathopen{}}
#1}
\\DeclarePairedDelimiterXPP\\condVar[2]{\\Var} (){}{
\\renewcommand\\given{\\nonscript\\:\\delimsize\\vert\\nonscript\\:\\mathopen{}}
#1,#2}
\\DeclarePairedDelimiterXPP\\condCov[2]{\\cov} (){}{
\\renewcommand\\given{\\nonscript\\:\\delimsize\\vert\\nonscript\\:\\mathopen{}}
#1,#2}
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
\\definecolor{bgcolorminted}{HTML}{2e3440}
\\usepackage{hyperref}
\\usepackage[]{cleveref}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
\\usemintedstyle{nord}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}"))))

(setq org-beamer-frame-level 2)

(setq org-beamer-theme "[progressbar=frametitle, titleformat=smallcaps, numbering=fraction]metropolis")

(with-eval-after-load 'ox-latex
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass[c]{beamer}
\\usepackage[american]{babel}
\\usetheme[progressbar=frametitle, titleformat=smallcaps, numbering=fraction]{metropolis}
\\usepackage{booktabs}
\\usepackage{float}
\\usepackage{mathtools}
\\usepackage{amsthm}
\\usepackage{amssymb}
\\usepackage[varbb]{newpxmath}
\\usepackage[]{xfrac}
\\usepackage{siunitx}
\\usepackage{graphicx}
\\usepackage{caption}
\\captionsetup{labelfont=bf,font={small,singlespacing}}
\\usepackage{subcaption}
\\usepackage{cancel}
\\usepackage{setspace}
\\usepackage{xcolor}
\\usepackage{diffcoeff}
\\usepackage{nicematrix}
\\usepackage{acronym}
\\usepackage{appendixnumberbeamer}
\\usepackage{dirtytalk}
\\usepackage{xurl}
\\DeclareMathOperator{\\Var}{Var}
\\DeclareMathOperator{\\cov}{Cov}
\\DeclareMathOperator{\\E}{\\mathbb{E}}
\\DeclareMathOperator*{\\argmax}{arg\\,max}
\\DeclareMathOperator*{\\argmin}{arg\\,min}
\\newcommand{\\Et}[2]{\\E_{#2} \\left[#1\\right]}
\\newcommand{\\Covt}[3]{\\cov_{#3}\\left(#1, #2\\right)}
\\newcommand{\\Vart}[2]{\\Var_{#2} \\left[#1\\right]}
\\DeclarePairedDelimiter\\abs{\\lvert}{\\rvert}
\\DeclarePairedDelimiter\\norm{\\lVert}{\\rVert}
\\DeclarePairedDelimiterX\\innerp[2]{\\langle}{\\rangle}{#1,#2}
\\DeclarePairedDelimiterX\\braket[3]{\\langle}{\\rangle}%
{#1\\,\\delimsize\\vert\\,\\mathopen{}#2\\,\\delimsize\\vert\\,\\mathopen{}#3}
\\providecommand\\given{}
\\DeclarePairedDelimiterXPP\\Prob[1]{\\mathbb{P}} (){}{
\\renewcommand\\given{\\nonscript\\:\\delimsize\\vert\\nonscript\\:\\mathopen{}}
#1}
\\DeclarePairedDelimiterXPP\\condE[1]{\\E} (){}{
\\renewcommand\\given{\\nonscript\\:\\delimsize\\vert\\nonscript\\:\\mathopen{}}
#1}
\\DeclarePairedDelimiterXPP\\condVar[2]{\\Var} (){}{
\\renewcommand\\given{\\nonscript\\:\\delimsize\\vert\\nonscript\\:\\mathopen{}}
#1,#2}
\\DeclarePairedDelimiterXPP\\condCov[2]{\\cov} (){}{
\\renewcommand\\given{\\nonscript\\:\\delimsize\\vert\\nonscript\\:\\mathopen{}}
#1,#2}
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
\\newtheorem{case}{Case}


\\definecolor{textcolor}{HTML}{2E3440}
\\definecolor{titlecolor}{HTML}{a3be8c}
\\definecolor{alertcolor}{HTML}{BF616A}
\\definecolor{examplecolor}{HTML}{EBCB8B}

\\definecolor{bgcolor}{HTML}{ECEFF4}
\\definecolor{barcolor}{HTML}{88C0D0}
\\definecolor{bgbarcolor}{HTML}{D8DEE9}
\\definecolor{bgcolorminted}{HTML}{2e3440}
\\setbeamercolor{progress bar}{fg=barcolor,bg=bgbarcolor}
\\setbeamercolor{frametitle}{fg=titlecolor,bg=bgcolor}
\\setbeamercolor{normal text}{fg=textcolor,bg=bgcolor}
\\setbeamercolor{alerted text}{fg=alertcolor,bg=bgcolor}
\\setbeamercolor{example text}{fg=examplecolor}
\\setbeamercovered{dynamic}
\\usecolortheme{rose}
\\usepackage{hyperref}
\\usepackage[]{cleveref}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
\\usemintedstyle{nord}"
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
(add-hook! 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

(use-package! engrave-faces-latex
  :after ox-latex)
(setq org-latex-src-block-backend 'engraved)
(setq org-latex-engraved-theme 'doom-nord-light)

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; (use-package! lsp-bridge
;;   :config
;;   (setq lsp-bridge-enable-log nil)
;;   (global-lsp-bridge-mode))

;; (after! lsp-mode
;;   (setq lsp-tex-server 'digestif))

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

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
          mu4e-compose-signature-auto-include nil   ; I don't want a message signature
          mu4e-use-fancy-chars t                   ; allow fancy icons for mail threads
          mu4e-context-policy 'pick-first   ;; Start with the first context
          mu4e-compose-context-policy 'ask) ;; Always ask which context to use when composing a new mail
  (setq mu4e-update-interval (* 1 60))
  (setq mu4e-attachment-dir "~/Downloads")
  (set-email-account! "gmail"
                      '((mu4e-sent-folder       . "/gmail/[Gmail]/Sent Mail")
                        (mu4e-drafts-folder     . "/gmail/[Gmail]/Drafts")
                        (mu4e-trash-folder      . "/gmail/[Gmail]/Trash")
                        (mu4e-refile-folder     . "/gmail/[Gmail]/All Mail")
                        (smtpmail-smtp-user     . "leoaparisi@gmail.com")
                        (mu4e-compose-signature . "---\nLeo Aparisi de Lannoy"))
                      t)
  (set-email-account! "U Chicago"
                      '((mu4e-sent-folder       . "/UChicago/Sent Mail")
                        (mu4e-drafts-folder     . "/UChicago/Drafts")
                        (mu4e-trash-folder      . "/UChicago/Trash")
                        (mu4e-refile-folder     . "/UChicago/All Mail")
                        (smtpmail-smtp-user     . "laparisidelannoy@uchicago.edu")
                        (mu4e-compose-signature . "---\nLeo Aparisi de Lannoy"))
                      t)
  (setq +mu4e-gmail-accounts '(("leoaparisi@gmail.com" . "/gmail/[Gmail]")))
  (setq mu4e-compose-dont-reply-to-self t)
  ;; Add a unified inbox shortcut
  (add-to-list
   'mu4e-bookmarks
   '(:name "Unified inbox" :query "maildir:/.*inbox/" :key ?i) t)
(add-hook 'mu4e-compose-mode-hook 'company-mode)
  )

;;(mu4e-alert-set-default-style 'notifier)
;;(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
