(package! theme-magic)

(package! info-colors)

(package! org-modern)

(package! org-appear)

;; (package! org-fragtog)

(package! org-super-agenda)

(package! org-roam-ui)
(package! websocket)

(package! org-diff
  :recipe (:host github
           :repo "tecosaur/orgdiff"))

(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! zotxt)

(package! org-chef)

(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))

(package! doct)

;; (package! lsp-ltex)
(package! eglot-ltex :recipe (:host github :repo "emacs-languagetool/eglot-ltex"))

(package! vlfi)
