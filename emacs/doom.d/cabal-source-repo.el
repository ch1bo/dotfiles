;;; cabal-source-repo.el --- Upsert source-repository-package stanzas -*- lexical-binding: t; -*-

(require 'project)
(require 'subr-x)

(defun cabal-source-repo--shell (dir program &rest args)
  "Run PROGRAM with ARGS in DIR; return trimmed stdout or signal."
  (with-temp-buffer
    (let* ((default-directory (file-name-as-directory (expand-file-name dir)))
           (exit (apply #'call-process program nil t nil args)))
      (unless (zerop exit)
        (error "`%s %s' failed in %s: %s"
               program (string-join args " ") dir (buffer-string)))
      (string-trim (buffer-string)))))

(defun cabal-source-repo--prefetch-hash (dir ref)
  "Return SRI hash from nix-prefetch-git on DIR @ REF."
  (message "nix-prefetch-git %s %s..." dir ref)
  (with-temp-buffer
    (let ((exit (call-process-shell-command
                 (format "nix-prefetch-git --quiet %s %s | jq -r .hash"
                         (shell-quote-argument (expand-file-name dir))
                         (shell-quote-argument ref))
                 nil t)))
      (unless (zerop exit)
        (error "nix-prefetch-git failed: %s" (buffer-string)))
      (string-trim (buffer-string)))))

(defun cabal-source-repo--to-https (url)
  "Rewrite SSH-style git URLs to https://."
  (cond
   ;; git@host:owner/repo(.git)
   ((string-match "\\`git@\\([^:]+\\):\\(.+\\)\\'" url)
    (format "https://%s/%s" (match-string 1 url) (match-string 2 url)))
   ;; ssh://[user@]host/owner/repo(.git)
   ((string-match "\\`ssh://\\(?:[^@/]+@\\)?\\([^/]+\\)/\\(.+\\)\\'" url)
    (format "https://%s/%s" (match-string 1 url) (match-string 2 url)))
   (t url)))

(defun cabal-source-repo--remotes (dir)
  "Return alist of (NAME . URL) for git remotes in DIR (fetch URLs)."
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory (expand-file-name dir))))
      (unless (zerop (call-process "git" nil t nil "remote" "-v"))
        (error "git remote -v failed in %s: %s" dir (buffer-string)))
      (let (result seen)
        (goto-char (point-min))
        (while (re-search-forward
                "^\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[ \t]+(fetch)" nil t)
          (let ((name (match-string 1)) (url (match-string 2)))
            (unless (member name seen)
              (push name seen)
              (push (cons name url) result))))
        (nreverse result)))))

(defun cabal-source-repo--remote-url (dir)
  "Pick a git remote in DIR and return its HTTPS URL."
  (let ((remotes (cabal-source-repo--remotes dir)))
    (cond
     ((null remotes)
      (error "No git remotes in %s" dir))
     ((= 1 (length remotes))
      (cabal-source-repo--to-https (cdar remotes)))
     (t
      (let* ((labels  (mapcar (lambda (r)
                                (format "%s  %s" (car r)
                                        (cabal-source-repo--to-https (cdr r))))
                              remotes))
             (default (or (seq-find (lambda (l) (string-prefix-p "origin " l)) labels)
                          (car labels)))
             (pick    (completing-read "Remote: " labels nil t nil nil default))
             (name    (car (split-string pick))))
        (cabal-source-repo--to-https (cdr (assoc name remotes))))))))

(defun cabal-source-repo--resolve-ref (dir ref)
  (cabal-source-repo--shell dir "git" "rev-parse" ref))

(defun cabal-source-repo--current-project-file ()
  "Return cabal.project at current project root, or signal."
  (let* ((proj (project-current))
         (root (if proj (project-root proj) default-directory))
         (cabal (expand-file-name "cabal.project" root)))
    (unless (file-exists-p cabal)
      (error "No cabal.project in %s" root))
    cabal))

(defun cabal-source-repo--package-name (cabal-file)
  (with-temp-buffer
    (insert-file-contents cabal-file)
    (goto-char (point-min))
    (if (re-search-forward "^[ \t]*name:[ \t]*\\([^ \t\n]+\\)" nil t)
        (match-string 1)
      (file-name-base cabal-file))))

(defun cabal-source-repo--expand-pattern (pattern root)
  "Expand a cabal.project PACKAGES entry PATTERN under ROOT to .cabal files."
  (let ((abs (expand-file-name pattern root)))
    (cond
     ((string-match-p "\\.cabal\\'" abs)
      (or (file-expand-wildcards abs)
          (and (file-exists-p abs) (list abs))))
     (t
      (let* ((dir  (directory-file-name abs))
             (dirs (if (string-match-p "[*?]" dir)
                       (seq-filter #'file-directory-p
                                   (file-expand-wildcards dir))
                     (and (file-directory-p dir) (list dir)))))
        (mapcan (lambda (d) (directory-files d t "\\.cabal\\'")) dirs))))))

(defun cabal-source-repo--collect-patterns (file visited)
  "Walk FILE and its `import:' chain; return (PATTERNS . VISITED').
  PATTERNS is a list of (DIR . PATTERN) cons cells, where DIR is the
  directory of the cabal-project file the pattern came from."
  (setq file (expand-file-name file))
  (if (or (member file visited) (not (file-exists-p file)))
      (cons nil visited)
    (push file visited)
    (let ((dir (file-name-directory file))
          patterns imports)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^packages:[ \t]*" nil t)
          (let* ((start (point))
                 (end   (save-excursion
                          (forward-line 1)
                          (while (and (not (eobp))
                                      (or (looking-at "^[ \t]")
                                          (looking-at "^[ \t]*\\(--.*\\)?$")))
                            (forward-line 1))
                          (point)))
                 (text (replace-regexp-in-string
                        "--[^\n]*" ""
                        (buffer-substring-no-properties start end))))
            (dolist (pat (split-string text "[ \t\n,]+" t))
              (push (cons dir pat) patterns))))
        (goto-char (point-min))
        (while (re-search-forward "^import:[ \t]*\\([^\n]+\\)" nil t)
          (let ((imp (string-trim
                      (replace-regexp-in-string
                       "--.*" "" (match-string 1)))))
            (unless (string-empty-p imp)
              (push (expand-file-name imp dir) imports)))))
      (setq patterns (nreverse patterns)
            imports  (nreverse imports))
      (dolist (imp imports)
        (let ((sub (cabal-source-repo--collect-patterns imp visited)))
          (setq patterns (append patterns (car sub))
                visited  (cdr sub))))
      (cons patterns visited))))

(defun cabal-source-repo--find-packages (repo-dir)
  "Return alist of (SUBDIR . PKGNAME) for all packages reachable from
  REPO-DIR/cabal.project, including via `import:' directives."
  (let ((cabal (expand-file-name "cabal.project" repo-dir)))
    (unless (file-exists-p cabal)
      (error "No cabal.project in %s" repo-dir))
    (let ((patterns (car (cabal-source-repo--collect-patterns cabal nil)))
          result)
      (dolist (p patterns)
        (let ((dir (car p)) (pat (cdr p)))
          (dolist (cf (cabal-source-repo--expand-pattern pat dir))
            (let* ((rel    (file-relative-name (file-name-directory cf) repo-dir))
                   (subdir (directory-file-name rel))
                   (pkg    (cabal-source-repo--package-name cf)))
              (push (cons (if (equal subdir ".") "" subdir) pkg) result)))))
      (delete-dups (nreverse result)))))

(defun cabal-source-repo--select-subdirs (packages)
  "Prompt user to multi-select subdirs from PACKAGES alist."
  (let* ((alist (mapcar (lambda (p)
                          (cons (format "%s  (%s)"
                                        (if (string-empty-p (car p)) "." (car p))
                                        (cdr p))
                                (car p)))
                        packages))
         (picks (completing-read-multiple
                 "Subdirs (comma-separated, TAB to complete): "
                 alist nil t)))
    (mapcar (lambda (p) (cdr (assoc p alist))) picks)))

(defun cabal-source-repo--stanza-bounds ()
  "If point is on a source-repository-package header, return (START . END)."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^source-repository-package\\b")
      (let ((start (point)))
        (forward-line 1)
        (while (and (not (eobp))
                    (or (looking-at "^[ \t]")
                        (looking-at "^[ \t]*\\(--.*\\)?$")))
          (forward-line 1))
        (cons start (point))))))

(defun cabal-source-repo--find-stanza (url)
  "Return (START . END) of stanza whose location matches URL, or nil."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
                  (re-search-forward "^source-repository-package\\b" nil t))
        (beginning-of-line)
        (let ((b (cabal-source-repo--stanza-bounds)))
          (when b
            (save-excursion
              (goto-char (car b))
              (when (re-search-forward
                     (format "^[ \t]+location:[ \t]*%s[ \t]*$"
                             (regexp-quote url))
                     (cdr b) t)
                (setq found b)))
            (goto-char (cdr b)))))
      found)))

(defun cabal-source-repo--update-stanza (bounds commit sha256)
  "Overwrite tag:/--sha256: within stanza at BOUNDS."
  (save-excursion
    (save-restriction
      (narrow-to-region (car bounds) (cdr bounds))
      (goto-char (point-min))
      (if (re-search-forward "^\\([ \t]+\\)tag:[ \t]*.*$" nil t)
          (replace-match (format "\\1tag: %s" commit) t nil)
        (error "No tag: field in existing stanza"))
      (goto-char (point-min))
      (if (re-search-forward "^\\([ \t]+\\)--sha256:[ \t]*.*$" nil t)
          (replace-match (format "\\1--sha256: %s" sha256) t nil)
        (goto-char (point-min))
        (re-search-forward "^\\([ \t]+\\)tag:.*$")
        (end-of-line)
        (insert (format "\n%s--sha256: %s" (match-string 1) sha256))))))

(defun cabal-source-repo--append-stanza (url commit sha256 subdirs)
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (unless (looking-back "\n\n" 2) (insert "\n"))
  (insert "source-repository-package\n"
          "    type: git\n"
          (format "    location: %s\n" url)
          (format "    tag: %s\n" commit)
          (format "    --sha256: %s\n" sha256))
  (when subdirs
    (insert "    subdir:\n")
    (dolist (s subdirs)
      (insert (format "        %s\n" (if (string-empty-p s) "." s))))))

;;;###autoload
(defun cabal-source-repo-upsert (repo-dir ref)
  "Upsert a source-repository-package stanza in the current project's
  cabal.project for REPO-DIR pinned at REF."
  (interactive
   (list (read-directory-name "Upstream repository: " nil nil t)
         (let ((r (read-string "Git ref [HEAD]: " nil nil "HEAD")))
           (if (string-empty-p r) "HEAD" r))))
  (let* ((repo-dir  (expand-file-name repo-dir))
         (cabal     (cabal-source-repo--current-project-file))
         (url       (cabal-source-repo--remote-url repo-dir))
         (commit    (cabal-source-repo--resolve-ref repo-dir ref))
         (sha256    (cabal-source-repo--prefetch-hash repo-dir commit))
         (buf       (find-file-noselect cabal))
         (bounds    (with-current-buffer buf
                      (cabal-source-repo--find-stanza url)))
         (subdirs   (unless bounds
                      (let ((pkgs (cabal-source-repo--find-packages repo-dir)))
                        (cond
                         ((null pkgs)
                          (error "No packages found in %s/cabal.project" repo-dir))
                         ((and (= 1 (length pkgs))
                               (string-empty-p (caar pkgs)))
                          nil)
                         (t
                          (cabal-source-repo--select-subdirs pkgs)))))))
    (with-current-buffer buf
      (save-excursion
        (if bounds
            (cabal-source-repo--update-stanza bounds commit sha256)
          (cabal-source-repo--append-stanza url commit sha256 subdirs)))
      (save-buffer))
    (message "%s source-repository-package: %s @ %s"
             (if bounds "Updated" "Added")
             url (substring commit 0 (min 12 (length commit))))))

(provide 'cabal-source-repo)
