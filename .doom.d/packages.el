(package! transient
      :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440"
      :recipe (:host github :repo "magit/transient"))

(package! with-editor
          :pin "bbc60f68ac190f02da8a100b6fb67cf1c27c53ab"
          :recipe (:host github :repo "magit/with-editor"))

(package! theme-magic)

(package! info-colors)

(package! org-modern)

(package! org-appear)

;; (package! org-fragtog)

;; (package! org-super-agenda)

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
;; (package! eglot-ltex :recipe (:host github :repo "emacs-languagetool/eglot-ltex"))

(package! vlfi)
