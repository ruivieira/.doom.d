;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Rui Vieira"
      user-mail-address "ruidevieira@googlemail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Hera" :size 14 :weight 'regular))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'acme)
;; (setq doom-theme 'doom-sourcerer)
;; (setq doom-theme 'doom-flatwhite)
;; (setq doom-theme 'doom-one-light)
;; (setq doom-theme 'doom-solarized-light)
;; (setq doom-theme 'doom-acario-dark)
;; (setq doom-theme 'tango-plus)
;; (setq doom-theme 'doom-Iosvkem)
(require 'uwu-theme)
(load-theme 'uwu t)

;; tree-sitter
(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(add-hook! 'solaire-mode-hook
  ;(set-face-attribute 'solaire-fringe-face nil :background (face-background 'solaire-hl-line-face))
  (set-face-attribute 'fringe nil :background (face-background 'solaire-default-face)))

(let ((alternatives
       ;; '("doom-emacs-color.png" "doom-emacs-color2.png" "doom-emacs-slant-out-bw.png" "doom-emacs-slant-out-color.png")
       '("spock.jpg")
       ))
  (setq fancy-splash-image
        (concat doom-private-dir "splash/"
                (nth (random (length alternatives)) alternatives))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq confirm-kill-emacs nil) ; Disable exit confirmation.

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Uncomment to enable fullscreen start
;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))

(after! f
  (setq org-agenda-files
      (flatten-list
       (mapcar #'(lambda (topic) (f-glob (format "~/Sync/notes/pages/%s/*.org" topic)))
              '("." "AI" "Code" "Code projects" "JIRAs" "Life" "Machine learning" "Tools")))))

;; Python configuration
(setq python-shell-completion-native-enable nil)
(use-package! python-black
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/Sync/notes/pages/")

(after! org
      (setq org-todo-keywords
            '((sequence "IDEA" "TODO" "LATER" "DOING" "|" "DONE" "CANCELED")
            (sequence "BACKLOG" "INPROGRESS" "ONHOLD" "INREVIEW" "|" "MERGED" "CANCELED")
            (sequence "WORK" "|" "DONE" "CANCELED")
            (sequence "SHOP" "|" "DONE" "CANCELED")
            (sequence "REVIEW" "|" "DONE" "CANCELED")
            (sequence "MEETING" "|" "MET" "CANCELED")))
      (setq org-agenda-custom-commands
            '(("l" todo "LATER")
            ("j" "Agenda and work tasks" (
                                          (agenda "")
                                          (todo "BACKLOG") (todo "INPROGRESS")
                                          (todo "WORK")
                                          (todo "REVIEW")))
            ("h" "Work (on hold): agend and tasks" (
                                                    (agenda "" ((todo "ONHOLD") (todo "INREVIEW")))
                                                    (todo "ONHOLD") (todo "INREVIEW")))
            ("m" "Work: agenda and meetings" (
                                              (agenda "" ((todo "MEETING")))
                                              (todo "MEETING")))
            ("w" "Work tasks and meetings" (
                                            (agenda "" ((org-agenda-span 14)
                                                        (todo "MEETING")
                                                        (todo "REVIEW")
                                                        (todo "BACKLOG")
                                                        (todo "WORK")
                                                        (tags "+work")))
                                    (todo "MEETING")
                                    (todo "REVIEW")
                                    (todo "BACKLOG")
                                    (todo "WORK")
                                    (tags "+work")))
    ("tw" tags-todo "+work")
    ("s" "Shopping: agenda and tasks" (
                                       (agenda "" ((todo "SHOP")))
                                       (todo "SHOP")))
    ("n" "General: agenda and TODOs" (
                             (agenda "" ((todo "TODO")))
                             (todo "TODO")))
    ("f" "Fortnight agenda and everything" (
                                            (agenda "" ((org-agenda-span 14)))
                                            (alltodo "")))
    ))
    (setq rui/org-agenda-directory "~/Sync/notes/pages/")
    (setq org-capture-templates
          `(("t" "todo" entry (file ,(concat rui/org-agenda-directory "Inbox.org"))
          "* TODO  %?")
         ("w" "work" entry (file ,(concat rui/org-agenda-directory "Inbox.org"))
            "* WORK  %?")
         ("e" "email" entry (file+headline ,(concat rui/org-agenda-directory "emails.org") "Emails")
          "* TODO  [#A] Reply: %a :@home:@school:" :immediate-finish t)
         ("l" "link" entry (file ,(concat rui/org-agenda-directory "Inbox.org"))
          "* TODO %(org-cliplink-capture)" :immediate-finish t)
         ("c" "org-protocol-capture" entry (file ,(concat rui/org-agenda-directory "Inbox.org"))
          "* TODO [[%:link][%:description]]\n\n %i" :immediate-finish t)))
 (setq org-image-actual-width nil)
 (setq org-confirm-babel-evaluate nil)  ;; skip org-babel confirmation dialog
 (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
 (setq org-preview-latex-default-process 'imagemagick)
 (org-babel-do-load-languages
  'org-babel-load-languages
  '(
    (emacs-lisp . t)
    (latex . t)
    (plantuml . t)
    (java . t)
    (python . t)
    (deno . t)
    (go . t)
    (crystal . t)
    (zig . t)
    (jupyter . t)))

 (setq org-roam-directory "~/Sync/notes/pages/"))

;;(after! org-src
;; (dolist (lang '(python typescript jupyter))
;; (cl-pushnew (cons (format "jupyter-%s" lang) lang)
;;                org-src-lang-modes :key #'car))
  ;; (org-babel-jupyter-override-src-block "python") ;; alias all python to jupyter-python
  ;; (org-babel-jupyter-override-src-block "typescript") ;; alias all python to jupyter-python
;; )

(add-hook 'typescript-mode-hook 'deno-fmt-mode)
(add-hook 'js2-mode-hook 'deno-fmt-mode)
;; (add-to-list 'org-src-lang-modes '("deno" . typescript))

;; Configure file templates
(set-file-template! "/post\\.org$" :trigger "__post.org" :mode 'org-mode)
(set-file-template! "/JIRAs\\.org$" :trigger "__jira.org" :mode 'org-mode)

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups '((:auto-dir-name t)))
  (org-super-agenda-mode))

(use-package! org-ref

  ;; this bit is highly recommended: make sure Org-ref is loaded after Org
  :after org

  ;; Put any Org-ref commands here that you would like to be auto loaded:
  ;; you'll be able to call these commands before the package is actually loaded.
  :commands
  (org-ref-cite-hydra/body
   org-ref-bibtex-hydra/body)

  ;; if you don't need any autoloaded commands, you'll need the following
  ;; :defer t

  ;; This initialization bit puts the `orhc-bibtex-cache-file` into `~/.doom/.local/cache/orhc-bibtex-cache
  ;; Not strictly required, but Org-ref will pollute your home directory otherwise, creating the cache file in ~/.orhc-bibtex-cache
  :init
  (let ((cache-dir (concat doom-cache-dir "org-ref")))
    (unless (file-exists-p cache-dir)
      (make-directory cache-dir t))
    (setq orhc-bibtex-cache-file (concat cache-dir "/orhc-bibtex-cache"))
    ))

(after! org
  (require 'org-ref))

(use-package! org-ref
    ;:after org-roam
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         bibtex-completion-bibliography (list "~/Sync/notes/references.bib")
         org-ref-note-title-format "* %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-function 'orb-edit-notes
    ))

(after! org-ref
(setq
 bibtex-completion-bibliography "~/Sync/notes/references.bib"
 bibtex-completion-pdf-field "file"
 bibtex-completion-notes-template-multiple-files
 (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journaltitle}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"
  )
 )
)
(after! ox-hugo
  (use-package! citeproc-org
    :config
    (citeproc-org-setup)
    (setq citeproc-org-org-bib-header "* References\n")
    )
  (setq org-hugo-auto-set-lastmod 't
      org-hugo-section "posts"
      org-hugo-suppress-lastmod-period 43200.0
      org-hugo-export-creator-string "Emacs 28.0 (Org mode 9.4 + ox-hugo)"
))

;; Special glyphs for org-babel blocks
(defun my/pretty-symbols ()
  (setq prettify-symbols-alist
          '(("#+begin_src python" . "🐍")
            ("#+begin_src elisp" . "λ")
            ("#+begin_src shell" . "🐚")
            ("#+begin_src jupyter-python" . "🪐 🐍")
            ("#+begin_src jupyter-java" . "🪐 ☕")
            ("#+end_src" . "―")
            ("#+results:" . "🔨")
            ("#+RESULTS:" . "🔨"))))

(add-hook 'clojure-mode-hook 'my/pretty-symbols)
(add-hook 'racket-mode-hook 'my/pretty-symbols)
(add-hook 'ess-mode-hook 'my/pretty-symbols)
(add-hook 'org-mode-hook 'my/pretty-symbols)
(global-prettify-symbols-mode +1)

(use-package humble)
(use-package quarkus)
(use-package kogito)
(use-package ob-zig)

;; (use-package eglot-java)


;; UI-related configurations
;;
;;; The important stuff
(blink-cursor-mode 1)                   ; A cursor should blink

;;; global beacon minor-mode
(use-package! beacon)
(after! beacon (beacon-mode 1))

;;; Focus package
(use-package! focus)

;;; set default indent
(setq-default tab-width 4)

;; System
(use-package! load-env-vars)

;; enable rainbow mode for lua
(add-hook 'lua-mode-hook #'rainbow-mode)
