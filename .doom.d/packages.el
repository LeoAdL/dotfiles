(package! org-block-capf :recipe (:host github :repo "xenodium/org-block-capf"))

(package! org-modern)

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(package! org-fragtog)

(package! org-vcard)

(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

;; (package! zotxt)

(package! org-chef)

(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))

(package! engrave-faces)

(package! doct
  :recipe (:host github :repo "progfolio/doct"))

(package! jinx)

(package! lsp-ltex)

(package! vlfi)

(package! csv-mode)

;; (package! mu4e :recipe (:host github :repo "emacsmirror/mu4e"))

(package! consult-mu :recipe (:host github :repo "armindarvish/consult-mu"))

;; (package! org-msg
;;   :recipe (:host github :repo "danielfleischer/org-msg" :branch "1.12")
;;   :pin "4dcd70f")

(package! browser-hist)

(package! youtube-sub-extractor)

(package! ultra-scroll-mac :recipe (:host github :repo "jdtsmith/ultra-scroll-mac"))

(package! evil-textobj-tree-sitter)
(package! treesit-auto)

(package! emacs-jupyter :recipe (:host github :repo "emacs-jupyter/jupyter"))

(package! emacs-eat  :recipe (
      :host codeberg
      :repo "akib/emacs-eat"
      :files ("*.el" ("term" "term/*.el") "*.texi"
              "*.ti" ("terminfo/e" "terminfo/e/*")
              ("terminfo/65" "terminfo/65/*")
              ("integration" "integration/*")
              (:exclude ".dir-locals.el" "*-tests.el"))))
