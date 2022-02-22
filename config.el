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
(setq doom-font (font-spec :family "Hera" :size 15 :weight 'regular))
(setq doom-variable-pitch-font (font-spec :family "ETBembo" :size 17 :weight 'regular))

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
;; (require 'uwu-theme)
;; (load-theme 'uwu t)
;; (setq doom-theme 'spacemacs-light)
(setq doom-theme 'modus-operandi)

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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/Sync/notes/pages/")


(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
    (load! "lisp/ob-scala"))


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

              ;; work commands
              ("wo" "Open tickets" (
                                    (org-ql-block '(and
                                                    (property "type" "JIRA")
                                                    (or (todo) (todo "LATER") (todo "WORK"))
                                                    ))
                                    ))
              ("wa" "Agenda and work tasks" (
                                             (agenda "")
                                             (todo "BACKLOG") (todo "INPROGRESS")
                                             (todo "WORK")
                                             (todo "REVIEW")))
              ("wh" "Work (on hold): agend and tasks" (
                                                    (agenda "" ((todo "ONHOLD") (todo "INREVIEW")))
                                                    (todo "ONHOLD") (todo "INREVIEW")))
              ("wm" "Work: agenda and meetings" (
                                              (agenda "" ((todo "MEETING")))
                                              (todo "MEETING")))
              ("ww" "Work tasks and meetings" (
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
 (setq org-babel-clojure-backend 'cider) ;; set backend for org-babel clojure blocks
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
    (clojure . t)
    (scala . t)
    (pikchr . t)
    (jupyter . t)))

 (setq org-roam-directory "~/Sync/notes/pages/")

 (use-package! org-ql)

 ;; org-mode styling
 (setq org-hide-emphasis-markers t)
 (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

   (let* ((variable-tuple
          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.2))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.3))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

   (custom-theme-set-faces
    'user
    '(variable-pitch ((t (:family "ETBembo" :height 170 :weight thin))))
    '(fixed-pitch ((t ( :family "Hack" :height 150)))))

   (add-hook 'org-mode-hook 'variable-pitch-mode)
   (add-hook 'org-mode-hook 'visual-line-mode)
   (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
 )


(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

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
            ("#+begin_src clojure" . "λ")
            ("#+begin_src shell" . "🐚")
            ("#+begin_src amm" . "🍄")
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


(use-package ob-ammonite
  :defer 1
  :config
  (use-package ammonite-term-repl)
  (setq ammonite-term-repl-auto-detect-predef-file nil)
  (setq ammonite-term-repl-program-args '("--no-remote-logging" "--no-default-predef" "--no-home-predef"))
  (defun my/substitute-sbt-deps-with-ammonite ()
    "Substitute sbt-style dependencies with ammonite ones."
    (interactive)
    (apply 'narrow-to-region (if (region-active-p) (my/cons-cell-to-list (region-bounds)) `(,(point-min) ,(point-max))))
    (goto-char (point-min))
    (let ((regex "\"\\(.+?\\)\"[ ]+%\\{1,2\\}[ ]+\"\\(.+?\\)\"[ ]+%\\{1,2\\}[ ]+\"\\(.+?\\)\"")
          (res))
      (while (re-search-forward regex nil t)
        (let* ((e (point))
               (b (search-backward "\"" nil nil 6))
               (s (buffer-substring-no-properties b e))
               (s-without-percent (apply 'concat (split-string s "%")))
               (s-without-quotes (remove-if (lambda (x) (eq x ?" ;"
                                                            ))
                                            s-without-percent))
               (s-as-list (split-string s-without-quotes)))
                    (delete-region b e)
          (goto-char b)
          (insert (format "import $ivy.`%s::%s:%s`" (first s-as-list) (second s-as-list) (third s-as-list)))
          )
        )
      res)
    (widen)))



;; UI-related configurations
;;
;;; The important stuff
(blink-cursor-mode 1)                   ; A cursor should blink
(scroll-bar-mode 1)

;;; Keybindings

;;;; Disable Alt in order to use hash and euro
(global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#")))

;; Make the modeline font smaller
(setq doom-modeline-height 1)
(set-face-attribute 'mode-line nil :height 130)
(set-face-attribute 'mode-line-inactive nil :height 130)

(use-package! dirvish)
;;; Global beacon minor-mode
;;; Uncomment the following two lines to enable beacon
;; (use-package! beacon)
;;(after! beacon (beacon-mode 1))

;;; Focus package
(use-package! focus)

;;; set default indent
(setq-default tab-width 4)

;; System
(use-package! load-env-vars)

;; enable rainbow mode for lua
(add-hook 'lua-mode-hook #'rainbow-mode)
