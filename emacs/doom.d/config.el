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
(setq doom-font (font-spec :family "HasklugNerdFont" :size 22 :weight 'normal))

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

;; Use Secrets API for managing secrets
(setq auth-sources '("secrets:Login"))

;; Quick key bindings (at least during transition to doom)
(setq which-key-idle-delay 0.2)

;; Don't follow symlinks by default
(setq find-file-visit-truename nil)

;; Keybindings .. mostly reminiscent of spacemacs muscle memory.
;;
;; Typically overrides of ../doom.emacs.d/modules/config/default/+evil-bindings.el
;; TODO: use :map to override only specifics
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
       :desc "Next error"           "n"   #'flycheck-next-error
       :desc "Previous error"       "p"   #'flycheck-previous-error
       :desc "Select checker"       "s"   #'flycheck-select-checker
       :desc "Verify setup"         "v"   #'flycheck-verify-setup
       ))

;; Set variables easily
(map! :map help-map
      "V" #'set-variable)

;; TODO: iedit/evil-multiedit aditional functionality: e.g. limit to line,
;; expand above / below

;; Local with-editor key bindings
(map! :map with-editor-mode-map
      :localleader
      "," #'with-editor-finish
      "f" #'with-editor-finish
      "k" #'with-editor-cancel)

;; Additional agenda mode key bindings
(map! :map org-agenda-mode-map
      :localleader
      :desc "Save org buffers" "s" #'org-save-all-org-buffers
      :desc "Reload org buffers" "r" #'org-revert-all-org-buffers
      :desc "Toggle deadlines" "!" #'org-agenda-toggle-deadlines)

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
(after! org
  (setq org-directory "~/sync/org/"
        org-default-notes-file (concat org-directory "notes.org")
        org-agenda-files (list org-directory)
        ;; REVIEW [[file:~/.dotfiles/emacs/doom.emacs.d/modules/lang/org/config.el::defun +org-init-capture-defaults-h (]]
        org-capture-templates
        '(("t" "Task" entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\nSCHEDULED: %t\n%a")
          ("i" "Idea" entry
           (file+headline org-default-notes-file "Ideas")
           "* IDEA %?\n%a")
          ("l" "Log" entry
           (file+headline org-default-notes-file "Log")
           "* %?\n%T\n%a")
          )
        org-agenda-custom-commands
        '(("d" agenda "Today"
           ((org-agenda-start-day "today")
            (org-agenda-span 'day)))
          ("n" "Full agenda"
           ((agenda ""
                    ((org-agenda-span 'week)))
            (tags-todo "task"
                       ((org-agenda-overriding-header "Tasks")
                        (org-agenda-skip-function
                         '(org-agenda-skip-subtree-if 'scheduled))))
            (tags-todo "idea"
                       ((org-agenda-overriding-header "Ideas")))
            )
           )
          ))

  ;; Inline css on org html export
  (add-hook 'org-export-before-processing-functions
            (lambda (exporter)
              (when (eq exporter 'html)
                (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
                       (path (concat dir "style.css"))
                       (homestyle (or (null dir) (null (file-exists-p path))))
                       (final (if homestyle "~/.dotfiles/emacs/org-style.css" path)))
                  (setq org-html-head-include-default-style nil)
                  (setq org-html-head (concat
                                       "<style type=\"text/css\">\n"
                                       "<!--/*--><![CDATA[/*><!--*/\n"
                                       (with-temp-buffer
                                         (insert-file-contents final)
                                         (buffer-string))
                                       "/*]]>*/-->\n"
                                       "</style>\n"))))))
  )

;; Timeclock

(after! timeclock
  (setq timeclock-file "~/sync/org/timelog"))

;; Unmap because by default global-display-fill-column-indicator-mode is bound to this
(map! :leader "t c" nil)
(map! :leader
      (:prefix "t" (:prefix ("c" . "clock")
                    :desc "Clock in" "i" #'timeclock-in
                    :desc "Clock out" "o" #'timeclock-out
                    :desc "Reread log" "r" #'timeclock-reread-log
                    :desc "Status" "s" #'timeclock-status-string
                    )))

;; LSP

(setq lsp-enable-file-watchers nil
      lsp-ui-doc-enable nil
      lsp-lens-enable nil
      lsp-treemacs-errors-position-params '((side . right)))

;; C/C++

(after! flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-clang-tidy-setup))

;; Groovy

(setq lsp-groovy-server-file
      (concat lsp-server-install-dir "groovy-language-server/groovy-language-server-all.jar"))

;; Haskell

;; Additional haskell mode key bindings
(map! :after haskell-mode
      :map haskell-mode-map
      :localleader
      "h" #'haskell-hoogle-lookup-from-local
      "H" #'haskell-hoogle)

;; Appropriate HLS is assumed to be in scope (by nix-shell)
(setq lsp-haskell-server-path "haskell-language-server"
      lsp-haskell-importlens-on nil
      lsp-haskell-plugin-import-lens-code-lens-on nil
      lsp-haskell-plugin-tactics-global-on nil
      lsp-haskell-plugin-stan-global-on nil
      lsp-response-timeout 30)

;; Don't' use lsp for formatting
(setq-hook! 'haskell-mode-hook +format-with-lsp nil)

;; TODO(SN): this is necessary as format-all-mode / format-all-buffer--from-hook
;; advice is not :override and had been broken the +onsave feature. So waiting
;; for that :editor format rewrite...
(defun add-autoformat-hook ()
  (add-hook 'before-save-hook '+format-buffer-h))
(add-hook! (haskell-mode haskell-cabal-mode) 'add-autoformat-hook)

;; Use 'cabal-fmt' for .cabal files
(set-formatter! 'cabal-fmt "cabal-fmt"
  :modes 'haskell-cabal-mode)

;; TODO How to organize formatters? brittany is default, and switching using
;; config updates is annoying.

;; Use 'fourmolu' as formatter.
(set-formatter!
  'fourmolu
  '("fourmolu" "--no-cabal")
  :modes 'haskell-mode
  :filter
  (lambda (output errput)
    (list output
          (replace-regexp-in-string "Loaded config from:[^\n]*\n*" "" errput))))

;; Use 'stylish-haskell' as formatter.
;;
;; NOTE Call stylish-haskell directly instead of the
;; 'haskell-mode-stylish-buffer command as I am still a bit puzzled why the
;; latter does not pick up the projects .stylish-haskell.yaml.
;; (set-formatter! 'stylish-haskell "stylish-haskell"
;;   :modes 'haskell-mode)

;; Purescript
(set-formatter! 'purty "purty"
  :modes 'purescript-mode)

;; Javascript

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))

;; MonkeyC

;; Use java-mode for "monkeyc" files, but disable auto-formatting
(add-to-list 'auto-mode-alist '("\\.mc\\'" . java-mode))
(after! format
  (nconc +format-on-save-enabled-modes '(java-mode)))

;; Rust

;; Don't' use lsp for formatting
(setq-hook! 'rustic-mode-hook +format-with-lsp nil)
(add-hook! (rustic-mode) 'add-autoformat-hook)

;; Nix
(set-formatter! 'nixpkgs-fmt "nixpkgs-fmt"
  :modes 'nix-mode)

;; Email

(setq +mu4e-backend 'offlineimap
      mu4e-maildir "~/mail"
      mml-secure-openpgp-sign-with-sender t
      mml-secure-openpgp-encrypt-to-self t
      mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND maildir:*INBOX")
;; Sign mails by default
(add-hook! 'mu4e-compose-mode-hook :append #'mml-secure-message-sign-pgpauto)
;; Load/Refresh main mu4e view on context change
(add-hook! 'mu4e-context-changed-hook #'mu4e)
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
   (mu4e-update-interval . 120)
   (mu4e-bookmarks . ((:name "Inbox" :query "maildir:/ncoding.at/INBOX" :key ?i)
                      (:name "Today's messages" :query "maildir:/ncoding.at/INBOX AND date:today..now" :key ?t)
                      (:name "Flagged messages" :query "maildir:/ncoding.at/* AND flag:flagged" :key ?f)
                      ))
   (mu4e-compose-signature . nil)
   ))
;; TODO DRY with mail/accounts-iohk.nix
(set-email-account!
 "iohk.io"
 '((user-mail-address . "sebastian.nagel@iohk.io")
   (mu4e-trash-folder . "/iohk.io/[Gmail].Trash")
   (mu4e-refile-folder  . "/iohk.io/[Gmail].All Mail") ;; TODO no archive?
   (mu4e-sent-folder . "/iohk.io/[Gmail].Sent Mail")
   (mu4e-drafts-folder . "/iohk.io/[Gmail].Drafts")
   (smtpmail-smtp-user . "sebastian.nagel@iohk.io")
   (smtpmail-smtp-server . "smtp.gmail.com")
   (smtpmail-smtp-service . 465)
   (smtpmail-stream-type . ssl)
   (mu4e-update-interval . 120)
   (mu4e-bookmarks . ((:name "Inbox" :query "maildir:/iohk.io/INBOX" :key ?i)
                      (:name "Today's messages" :query "maildir:/iohk.io/INBOX AND date:today..now" :key ?t)
                      (:name "Flagged messages" :query "maildir:/iohk.io/* AND flag:flagged" :key ?f)
                      (:name "Meetings" :query "maildir:/iohk.io/meetings" :key ?m)
                      ))
   (mu4e-compose-signature . (with-temp-buffer (insert-file-contents "~/.dotfiles/mail/iohk.sig") (buffer-string)))
   ))

;; Git auto-commit
(after! git-auto-commit-mode
  (setq gac-debounce-interval 5.0))
