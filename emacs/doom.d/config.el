;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Sebastian Nagel"
      user-mail-address "sebastian.nagel@ncoding.at")

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
(setq doom-font (font-spec :family "Source Code Pro" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Quick key bindings (at least during transition to doom)
(setq which-key-idle-delay 0.2)

;; Keybinding overrides of doom.emacs.d/modules/config/default/+evil-bindings.el
(map! :leader
      :desc "M-x"                   "SPC" #'execute-extended-command
      :desc "Search in project"     "/"   #'+default/search-project
      :desc "Comment lines"         ";"   #'comment-or-uncomment-region
      ;;; <leader> a --- agenda
      :desc "Org agenda"            "a"   #'org-agenda
      ;;; <leader> g --- git/version control
      (:prefix ("g" . "git")
          :desc "Git status"        "s"   #'magit-status
          ))
;; Local with-editor key bindings
(map! :map with-editor-mode-map
      :localleader
      "," #'with-editor-finish
      "f" #'with-editor-finish
      "k" #'with-editor-cancel)

;; Org
;; TODO(SN): keybindings in org-agenda (e.g. org-agenda-later)
(setq org-directory "~/documents/org/"
      org-default-notes-file (concat org-directory "notes.org")
      org-agenda-files (list org-directory)
      org-capture-templates
      '(("t" "Todo" entry (file+headline "notes.org" "Tasks") "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a")
        ("i" "Idea" entry (file+headline "notes.org" "Ideas") "* TODO %?%a")
        ("m" "Meeting" entry (file+headline "notes.org" "Meetings") "* %?\n  #+DATE: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))")
        )
      org-agenda-custom-commands
      '(("n" "Agenda and all TODOs"
         ((agenda ""
                  ((org-agenda-span
                    (quote day))))
          (tags-todo "-idea&-candidate"
                     ((org-agenda-overriding-header "TODOs (unscheduled)")
                      (org-agenda-skip-function
                       (quote
                        (org-agenda-skip-subtree-if
                         (quote scheduled))))))
          (tags-todo "idea"
                     ((org-agenda-overriding-header "Ideas"))))
         )))

;; Enable use of emacsclient
;;(server-start)

;; Language environment settings
(setq lsp-groovy-server-file
      (concat lsp-server-install-dir "groovy-language-server/groovy-language-server-all.jar"))

;; Use java-mode for "monkeyc" files, but disable auto-formatting
(add-to-list 'auto-mode-alist '("\\.mc\\'" . java-mode))
(after! format
  (nconc +format-on-save-enabled-modes '(java-mode)))

;; Don't follow symlinks by default
(setq find-file-visit-truename nil)
