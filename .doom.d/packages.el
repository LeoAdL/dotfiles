(package! theme-magic)

(package! info-colors)

(package! org-modern)

;; (package! org-roam-ui)
;; (package! websocket)

(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))

(package! zotxt)

(package! org-chef)

(package! org-cite-csl-activate :recipe (:host github :repo "andras-simonyi/org-cite-csl-activate"))

(package! engrave-faces)

(package! doct)

(package! jinx)

;; (package! lsp-ltex)
(package! eglot-ltex :recipe (:host github :repo "emacs-languagetool/eglot-ltex"))

(package! vlfi)
