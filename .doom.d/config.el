;;; config.el -*- lexical-binding: t; -*-

;; brew tap railwaycat/emacsmacport
;; brew install emacs-mac --with-mac-metal --with-natural-title-bar --with-native-compilation --with-xwidget

(setq user-full-name "Leo Aparisi de Lannoy"
      user-mail-address "leoaparisi@gmail.com")

(after! evil
(setq evil-want-fine-undo t)
(setq scroll-margin 99999)
(setq scroll-conservatively 0)
(setq maximum-scroll-margin 0.5)
(advice-add 'evil-scroll-up :after #'evil-scroll-line-to-center   )
(advice-add 'evil-scroll-down :after #'evil-scroll-line-to-center   )
)                       ; By default while in insert all changes are one big blob. Be more granular

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
