(package! org-block-capf :recipe (:host github :repo "xenodium/org-block-capf"))

(package! org-modern)
(package! org-superstar :disable t)
(package! org-bullets :disable t)

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(package! org-fragtog)

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

(package! doct)

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

(package! org-msg :recipe (:host github :repo "jeremy-compostella/org-msg" :branch "jcompost/mu-1.12-support-with-backward-compatibility") :pin "cc25647a43bf8f632b3a4cc8e85dc334a032e524")
