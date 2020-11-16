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
(setq display-line-numbers-type 't)

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

;; General

;; Quick key bindings (at least during transition to doom)
(setq which-key-idle-delay 0.2)

;; Don't follow symlinks by default
(setq find-file-visit-truename nil)

;; Keybindings .. mostly reminiscent of spacemacs muscle memory.
;;
;; Typically overrides of ../doom.emacs.d/modules/config/default/+evil-bindings.el
(map! :leader
      :desc "M-x"                   "SPC" #'execute-extended-command
      :desc "Search in project"     "/"   #'+default/search-project
      :desc "Comment lines"         ";"   #'comment-or-uncomment-region
      ;;; <leader> a --- agenda
      :desc "Org agenda"            "a"   #'org-agenda
      ;;; <leader> g --- git/version control
      (:prefix ("g" . "git")
       :desc "Git status"           "s"   #'magit-status
       )
      ;;; <leader> e --- errors (flycheck)
      (:prefix ("e" . "errors")
       :desc "List errors"          "l"   #'+toggle-flycheck-error-list
       :desc "Next error"           "n"   #'next-error
       :desc "Previous error"       "p"   #'previous-error
       :desc "Select checker"       "s"   #'flycheck-select-checker
       :desc "Verify setup"         "v"   #'flycheck-verify-setup
       ))

;; Local with-editor key bindings
(map! :map with-editor-mode-map
      :localleader
      "," #'with-editor-finish
      "f" #'with-editor-finish
      "k" #'with-editor-cancel)

;; Flycheck

(defun +toggle-flycheck-error-list ()
  "Toggle flycheck's error list window. If the error list is
visible, hide it. Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

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

;; Groovy

(setq lsp-groovy-server-file
      (concat lsp-server-install-dir "groovy-language-server/groovy-language-server-all.jar"))

;; Haskell

;; Use 'stylish-haskell' as formatter.
;;
;; NOTE: Call stylish-haskell directly instead of the
;; 'haskell-mode-stylish-buffer command as I am still a bit puzzled why the
;; latter does not pick up the projects .stylish-haskell.yaml.
(set-formatter! 'stylish-haskell "stylish-haskell"
  :modes 'haskell-mode)

;; MonkeyC

;; Use java-mode for "monkeyc" files, but disable auto-formatting
(add-to-list 'auto-mode-alist '("\\.mc\\'" . java-mode))
(after! format
  (nconc +format-on-save-enabled-modes '(java-mode)))

;; Email

(setq +mu4e-backend 'offlineimap
      mu4e-maildir "~/mail"
      mu4e-update-interval 120)

;; Load/Refresh main mu4e view on context change
(add-hook! 'mu4e-context-changed-hook 'mu4e)
;; TODO Update bookmarks: mails are only moved, not flagged as trash
;; TODO DRY with mail/accounts-franka.nix
(set-email-account!
 "franka.de"
 '((user-mail-address . "sebastian.nagel@franka.de")
   (mu4e-trash-folder . "/franka.de/Trash")
   (mu4e-refile-folder  . "/franka.de/Archive")
   (mu4e-sent-folder . "/franka.de/Sent")
   (mu4e-drafts-folder . "/franka.de/Drafts")
   (smtpmail-smtp-user . "sebastian.nagel@franka.de")
   (smtpmail-smtp-server . "mail.franka.de")
   (smtpmail-smtp-service . 25)
   (smtpmail-stream-type . starttls)
   (mu4e-bookmarks . ((:name "Unread messages" :query "maildir:/franka.de/* AND flag:unread AND NOT flag:trashed" :key ?u)
                      (:name "Today's messages" :query "maildir:/franka.de/* AND date:today..now AND NOT flag:trashed" :key ?t)
                      (:name "Flagged messages" :query "maildir:/franka.de/* AND flag:flagged" :key ?f)
                      ))
   )
 t)

;; TODO DRY with mail/accounts-ncoding.nix
(set-email-account!
 "ncoding.at"
 '((user-mail-address . "sebastian.nagel@ncoding.at")
   (mu4e-trash-folder . "/ncoding.at/Trash")
   (mu4e-refile-folder  . "/ncoding.at/Archive")
   (mu4e-sent-folder . "/ncoding.at/Sent")
   (mu4e-drafts-folder . "/ncoding.at/Drafts")
   (smtpmail-smtp-user . "sebastian.nagel@ncoding.at")
   (smtpmail-smtp-server . "mail.ncoding.at")
   (smtpmail-smtp-service . 465)
   (smtpmail-stream-type . ssl)
   (mu4e-bookmarks . ((:name "Unread messages" :query "maildir:/ncoding.at/* AND flag:unread AND NOT subject:SPAM" :key ?u)
                      (:name "Today's messages" :query "maildir:/ncoding.at/* AND date:today..now AND NOT subject:SPAM" :key ?t)
                      (:name "Flagged messages" :query "maildir:/ncoding.at/* AND flag:flagged" :key ?f)
                      ))
   ))
