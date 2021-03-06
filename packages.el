;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; UI-related packages
;;
;;; Themes
(package! acme-theme)
(package! doom-themes)
(package! tango-plus-theme)
(package! uwu-theme :recipe (:host github :repo "kborling/uwu.el"))
(package! spacemacs-theme)
(package! gruber-darker-theme)
(package! solo-jazz-theme)
;; general org-mode packages
(package! org-super-agenda)
(package! org-sidebar)
(package! org-ql)
(package! org-modern)
(package! org-randomnote)
(package! org-journal)
;; org-babel packages
(package! ob-deno)
(package! ob-go)
(package! ob-ammonite)
(package! ivy-bibtex)
(package! org-ref)
(package! citeproc-org)
(package! stimmung-themes)
;; Extra Python packages
(package! deno-fmt)

;;; Editing
(package! beacon)
(package! focus)
;; (unpin! org-roam)

;; Utilities
(package! f)
(package! deadgrep)
;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; org-mode related packages
;;
(package! org-extras
  :recipe (:host github :repo "ruivieira/elisp"
           :files ("org-extras.el")))

;; org-babel special modes
(package! ob-crystal)
(package! ob-zig
  :recipe (:host github :repo "jolby/ob-zig.el"
           :files ("*.el")))
(package! org-bullets)
;; Personal experimental packages

(package! humble
  :recipe (:host github :repo "ruivieira/elisp"
           :files ("humble.el")))

(package! quarkus
  :recipe (:host github :repo "ruivieira/elisp"
           :files ("quarkus.el")))

(package! kogito
  :recipe (:host github :repo "ruivieira/elisp"
           :files ("kogito.el")))

;;;; Programming languages

;; Python
(package! virtualenvwrapper)

(package! eglot-java
  :recipe (:host github :repo "emacs-vault/eglot-java"
           :files ("eglot-java.el")))
;; Fennel
(package! fennel-mode
  :recipe (:host github :repo "emacs-vault/fennel-mode"
           :files ("*.el")))

(package! janet-mode
  :recipe (:host github :repo "emacs-vault/janet-mode"
           :files ("*.el")))
(package! ijanet-mode
  :recipe (:host github :repo "emacs-vault/ijanet-mode"
           :files ("*.el")))

;; xonsh
(package! xonsh-mode)

;;;; IRC

;; circe
(package! circe)

;;;; Diagrams, plotting and graphs
;; plantuml-mode
(package! plantuml-mode)

;; tree-sitter support
(package! tree-sitter)
(package! tree-sitter-langs)

;; SQLite (and other DBs) support
(package! sqlite)

;; system
(package! load-env-vars)
(package! bug-hunter)

;; UI
(package! mini-frame)
(package! corfu)
(package! orderless)
(package! dirvish)
;;
;;
;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
