(package! org-block-capf :recipe (:host github :repo "xenodium/org-block-capf"))

(package! org-modern)
(package! org-superstar :disable t)
(package! org-bullets :disable t)

(package! org-fragtog)

(package! org-vcard)

;; (package! org-roam-ui)
;; (package! websocket)

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

;; (when (package! lsp-bridge
;;         :recipe (:host github
;;                  :repo "manateelazycat/lsp-bridge"
;;                  :branch "master"
;;                  :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;                  ;; do not perform byte compilation or native compilation for lsp-bridge
;;                  :build (:not compile)))
;;   (package! markdown-mode)
;;   (package! yasnippet))
;;   (package! lsp-mode :disable t)
;;   (package! company :disable t)
;;   (package! corfu :disable t)

(package! lsp-ltex)

;; (package! vlfi)

(package! consult-mu :recipe (:host github :repo "armindarvish/consult-mu"))

(package! org-msg :recipe (:host github :repo "danielfleischer/org-msg") :pin "19b7af2f31f030f8de3fa5be0634cdccb0335d96")

(package! browser-hist)

(package! youtube-sub-extractor)
