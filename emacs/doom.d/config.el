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

;; No mixed pitch fonts
(setq +zen-mixed-pitch-modes nil)

;; Configure formatting
(setq apheleia-formatters-respect-indent-level nil)

;; Keybindings
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
      (:prefix "g"
       :desc "Git status"           "s"   #'magit-status)
      ;;; <leader> e --- errors (flycheck)
      (:prefix "e"
       :desc "List errors"          "l"   #'+toggle-flycheck-error-list
       :desc "Next error"           "n"   #'flycheck-next-error
       :desc "Previous error"       "p"   #'flycheck-previous-error
       :desc "Select checker"       "s"   #'flycheck-select-checker
       :desc "Verify setup"         "v"   #'flycheck-verify-setup
       :desc "All errors"           "a"   #'lsp-treemacs-errors-list))

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

;; Magit

(use-package! magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil))

(map!
 :leader
 :prefix "p"
 :desc "List todos" "t" #'magit-todos-list)

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

;; LSP

(setq lsp-enable-file-watchers nil
      lsp-ui-doc-enable nil
      lsp-lens-enable nil
      lsp-treemacs-errors-position-params '((side . right)))

;; Generally not format with LSP
(setq +format-with-lsp nil)

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

;; Use 'cabal-fmt' for .cabal files
(set-formatter! 'cabal-fmt '("cabal-fmt") :modes '(haskell-cabal-mode))

;; Add 'stylish-haskell' as formatter.
(set-formatter! 'stylish-haskell '("stylish-haskell") :modes '(haskell-mode))

;; Use 'fourmolu' as formatter (keep this the last one)
(set-formatter! 'fourmolu '("fourmolu" "--stdin-input-file" filepath) :modes '(haskell-mode))

;; TODO How to organize formatters? brittany is default, and switching using
;; config updates is annoying.

;; Purescript

(set-formatter! 'purty '("purty") :modes '(purescript-mode))

;; Javascript

(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))

;; MonkeyC

;; Use java-mode for "monkeyc" files, but disable auto-formatting
(add-to-list 'auto-mode-alist '("\\.mc\\'" . java-mode))
(add-hook! 'java-mode-hook
           '((c-set-style "doom")
             (setq c-basic-offset 2)))

;; Nix

(set-formatter! 'nixpkgs-fmt '("nixpkgs-fmt") :modes '(nix-mode))

;; Aiken (Cardano)

(use-package! aiken-mode)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '("\\.ak\\'" . "aiken"))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("aiken" "lsp"))
                    :activation-fn (lsp-activate-on "aiken")
                    :server-id 'aiken-lsp)))

;; Mermaid
;; TODO: upstream as doom module
(use-package! ob-mermaid
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t)))
  )

;; Email

(setq +mu4e-backend 'offlineimap ;; XXX: Remove once mu4e-backend does not default to mbsync
      mu4e-maildir "~/mail"
      mml-secure-openpgp-sign-with-sender t
      mml-secure-openpgp-encrypt-to-self t
      mu4e-alert-interesting-mail-query "flag:unread AND NOT flag:trashed AND maildir:*INBOX")
;; use msmtp
(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail))
;; Sign mails by default
(add-hook! 'mu4e-compose-mode-hook :append #'mml-secure-message-sign-pgpauto)
;; Load/Refresh main mu4e view on context change
(add-hook! 'mu4e-context-changed-hook #'mu4e)
;; TODO DRY with mail/accounts-ncoding.nix
(set-email-account!
 "ncoding.at"
 '((user-mail-address . "sebastian.nagel@ncoding.at")
   (+mu4e-personal-addresses . (("sebastian.nagel@ncoding.at" "sebastian.nagel@ncoding.li")))
   (mu4e-trash-folder . "/ncoding.at/Trash")
   (mu4e-refile-folder  . "/ncoding.at/Archive")
   (mu4e-sent-folder . "/ncoding.at/Sent")
   (mu4e-drafts-folder . "/ncoding.at/Drafts")
   (smtpmail-smtp-user . "sebastian.nagel@ncoding.at")
   (smtpmail-smtp-server . "mail.ncoding.at")
   (smtpmail-smtp-service . 465)
   (smtpmail-stream-type . ssl)
   (smtpmail-servers-requiring-authorization . "mail\\.ncoding\\.at")
   (mu4e-update-interval . 120)
   (mu4e-bookmarks . ((:name "Inbox" :query "maildir:/ncoding.at/INBOX" :key ?i)
                      (:name "Today" :query "maildir:/ncoding.at/INBOX AND date:today..now" :key ?t)
                      (:name "Flagged" :query "maildir:/ncoding.at/* AND flag:flagged" :key ?f)
                      (:name "Archive" :query "maildir:/ncoding.at/Archive" :key ?a)
                      ))
   (mu4e-compose-signature . nil)
   ))
;; TODO DRY with mail/accounts-iohk.nix
(set-email-account!
 "iohk.io"
 '((user-mail-address . "sebastian.nagel@iohk.io")
   (mu4e-trash-folder . "/iohk.io/[Gmail].Trash")
   (mu4e-refile-folder  . "/iohk.io/[Gmail].All Mail")
   (mu4e-sent-folder . "/iohk.io/[Gmail].Sent Mail")
   (mu4e-drafts-folder . "/iohk.io/[Gmail].Drafts")
   (smtpmail-smtp-user . "sebastian.nagel@iohk.io")
   (smtpmail-smtp-server . "smtp.gmail.com")
   (smtpmail-smtp-service . 465)
   (smtpmail-stream-type . ssl)
   (mu4e-update-interval . 120)
   (mu4e-bookmarks . ((:name "Inbox" :query "maildir:/iohk.io/INBOX" :key ?i)
                      (:name "Today" :query "maildir:/iohk.io/INBOX AND date:today..now" :key ?t)
                      (:name "Flagged" :query "maildir:/iohk.io/* AND flag:flagged" :key ?f)
                      (:name "Meetings" :query "maildir:/iohk.io/meetings" :key ?m)
                      (:name "Archive" :query "maildir:\"/iohk.io/[Gmail].All Mail\"" :key ?a)
                      ))
   (mu4e-compose-signature . (with-temp-buffer (insert-file-contents "~/.dotfiles/mail/iohk.sig") (buffer-string)))
   ))

;; Git auto-commit
(after! git-auto-commit-mode
  (setq gac-debounce-interval 5.0))

;; Unfill paragraph
;;
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(map! :desc "Undo fill-paragraph" "C-q" #'unfill-paragraph)

;; Copilot
;;
;; From https://github.com/copilot-emacs/copilot.el
;; Accept completion from copilot and fallback to company
(use-package! copilot
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-j" . 'copilot-next-completion)
              ("C-k" . 'copilot-previous-completion))
  :config
  (setq copilot-indent-offset-warning-disable t))
(map! (:leader
       :prefix "t"
       :desc "Copilot" "c" 'copilot-mode))

;; Github-like markdown preview
;;
;; https://blog.bitsandbobs.net/blog/emacs-markdown-live-preview/
(use-package! impatient-mode
  :commands impatient-mode)

(defun github-markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun github-markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'github-markdown-filter)
  (imp-visit-buffer))

;; Obsidian configuration
;; TODO: Explore / extend this?
;; (use-package! obsidian
;;   :config
;;   (obsidian-specify-path "~/obsidian/Personal/")
;;   (global-obsidian-mode t)
;;   ;; This directory will be used for `obsidian-capture' if set.
;;   (obsidian-inbox-directory "Inbox")
;;   ;; The directory for daily notes (file name is YYYY-MM-DD.md)
;;   (obsidian-daily-notes-directory "Daily")
;;   ;; Directory of note templates, unset (nil) by default
;;   (obsidian-templates-directory "Templates")
;;   ;; Daily Note template name - requires a template directory. Default: Daily Note Template.md
;;   (obsidian-daily-note-template "Daily.md"))
