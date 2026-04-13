;;; async-installer.el --- Async package install/update from Git repos -*- lexical-binding: t; -*-

;; Author: Fujisawa Electric Management Office
;; URL: https://github.com/zawatton21/async-installer
;; Version: 0.3.0
;; Keywords: lisp, tools, convenience
;; Package-Requires: ((emacs "27.1") (async "1.9"))

;;; Commentary:
;;
;; async-installer provides non-blocking package installation and updates
;; for Emacs, using background processes.  Packages can be installed and
;; updated while you continue editing — no waiting, no restart required.
;;
;; Two installation backends:
;;
;; 1. Git — `async-installer-git-install-all-interactive' clones repos
;;    from any Git host (GitHub, GitLab, Codeberg, self-hosted, etc.)
;;    with commit/branch/tag pinning and optional pre-build commands.
;;    After cloning, missing dependencies are automatically detected from
;;    Package-Requires headers and reported.
;; 2. Archive — `async-installer-archive-start' downloads and installs
;;    tar/gz/zip archives from URLs.
;;
;; Any Git repository URL works — GitHub, GitLab, Codeberg, Gitea,
;; SourceHut, self-hosted instances, or any URL that `git clone' accepts.
;;
;; Philosophy — plain elisp over macro DSLs:
;;
;; This package is designed for users who manage packages with plain
;; `require' and elisp rather than macro-based package managers
;; (use-package, straight.el, elpaca, leaf, etc.).
;;
;; Plain elisp configuration is:
;; - Transparent: `require', `load', `add-to-list' are standard Emacs
;;   Lisp — no DSL to learn, no macro expansion to debug.
;; - AI-friendly: LLMs can read and generate standard elisp far more
;;   reliably than macro-specific syntax with implicit behavior.
;; - Portable: your config is just elisp; it works on any Emacs
;;   without bootstrapping a package manager first.
;;
;; Usage:
;;
;; ;; Git packages (with version pinning — any host works)
;; (async-installer-git-add "https://github.com/org/repo.git"
;;                          :commit "abc1234")
;; (async-installer-git-add "https://gitlab.com/org/repo.git")
;; (async-installer-git-add "https://codeberg.org/org/repo.git")
;; (async-installer-git-install-all-interactive)
;;
;; ;; Update all Git packages
;; (async-installer-git-update-all-interactive)

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'async)

;; ============================================================
;; Utilities
;; ============================================================

(defun async-installer--emacs-binary ()
  "Return the path to the current Emacs binary."
  (concat (replace-regexp-in-string "\\\\" "/" invocation-directory)
          invocation-name))

;; ============================================================
;; Archive async install (tar/gz/zip from URL)
;; ============================================================

(defvar async-installer-archive-list nil
  "List of archive URLs to download and install.")

(defvar async-installer-archive-max-concurrent 3
  "Maximum concurrent archive downloads.")

(defvar async-installer-archive--running 0
  "Current number of running archive processes.")

(defvar async-installer-archive--queue nil
  "Internal queue of URLs being processed.")

;;;###autoload
(defun async-installer-archive-add (&rest urls)
  "Add one or more archive URLS to the download list."
  (dolist (url urls)
    (add-to-list 'async-installer-archive-list url)))

(defun async-installer-archive--download-command ()
  "Return (PROGRAM . ARGS) for downloading files."
  (cond
   ((executable-find "curl")  '("curl" "-f" "-L" "-O"))
   ((eq system-type 'windows-nt) '("powershell" "Start-BitsTransfer"))
   ((executable-find "wget")  '("wget"))
   (t (error "No download tool found (need curl, wget, or powershell)"))))

(defun async-installer-archive--start-download (url sentinel-func)
  "Download URL asynchronously, call SENTINEL-FUNC on completion."
  (let* ((filename (file-name-nondirectory url))
         (cmd (async-installer-archive--download-command))
         (proc-name (format "download-%s" filename))
         (proc-buf (format "*download-%s*" filename)))
    (message "[async-archive] Downloading: %s" url)
    (start-process proc-name proc-buf (car cmd)
                   (append (cdr cmd) (list url)))
    (set-process-sentinel (get-process proc-name) sentinel-func)))

(defun async-installer-archive--decompress-command (file)
  "Return shell command to decompress FILE, or nil if .tar."
  (let ((lower (downcase file)))
    (cond
     ((string-match-p "\\.tar\\'" lower) nil)
     ((string-match-p "\\.tar\\.gz\\'\\|\\.tgz\\'" lower)
      (format "gzip -dc %S | tar xf -" file))
     ((string-match-p "\\.tar\\.xz\\'\\|\\.txz\\'" lower)
      (format "xz -dc %S | tar xf -" file))
     ((string-match-p "\\.tar\\.bz2\\'\\|\\.tbz\\'" lower)
      (format "bzip2 -dc %S | tar xf -" file))
     ((string-match-p "\\.tar\\.lz\\'" lower)
      (format "lzip -dc %S | tar xf -" file))
     ((string-match-p "\\.zip\\'" lower)
      (format "unzip -o %S" file))
     (t nil))))

(defun async-installer-archive--output-tar-filename (orig-file)
  "Guess the .tar filename after decompression of ORIG-FILE."
  (let* ((case-fold-search t)
         (tmp (replace-regexp-in-string
               "\\.gz\\'\\|\\.xz\\'\\|\\.bz2\\'\\|\\.lz\\'\\|\\.zip\\'" "" orig-file)))
    (unless (string-match-p "\\.tar\\'" tmp)
      (setq tmp (concat tmp ".tar")))
    tmp))

(defun async-installer-archive--maybe-decompress (filename callback)
  "Decompress FILENAME if needed, then call CALLBACK with (ok result-file)."
  (let ((cmd (async-installer-archive--decompress-command filename)))
    (if (null cmd)
        (funcall callback t filename)
      (let* ((proc-name (format "decompress-%s" (file-name-nondirectory filename)))
             (proc-buf (format "*decompress-%s*" (file-name-nondirectory filename))))
        (message "[async-archive] Decompressing: %s" filename)
        (start-process-shell-command proc-name proc-buf cmd)
        (set-process-sentinel
         (get-process proc-name)
         (lambda (proc _event)
           (if (and (eq (process-status proc) 'exit)
                    (zerop (process-exit-status proc)))
               (let ((out (async-installer-archive--output-tar-filename filename)))
                 (if (file-exists-p out)
                     (funcall callback t out)
                   (funcall callback nil (format "No output .tar: %s" out))))
             (funcall callback nil (format "Decompress failed: %s" filename)))))))))

(defun async-installer-archive--batch-install (filename sentinel-func)
  "Install FILENAME via batch Emacs, call SENTINEL-FUNC on completion."
  (let* ((proc-name (format "batch-install-%s" (file-name-nondirectory filename)))
         (proc-buf (format "*batch-install-%s*" (file-name-nondirectory filename))))
    (message "[async-archive] Batch installing: %s" filename)
    (apply #'start-process proc-name proc-buf
           (async-installer--emacs-binary)
           "--batch" "-Q"
           "--eval" "(require 'package)"
           "--eval" "(setq package-check-signature nil)"
           "--eval" (format "(ignore-errors (package-install-file %S))"
                            (expand-file-name filename))
           nil)
    (set-process-sentinel (get-process proc-name) sentinel-func)))

(defun async-installer-archive--process-url (url)
  "Download, decompress, and install archive from URL."
  (async-installer-archive--start-download
   url
   (lambda (proc _event)
     (if (and (eq (process-status proc) 'exit)
              (zerop (process-exit-status proc)))
         (let ((filename (file-name-nondirectory url)))
           (async-installer-archive--maybe-decompress
            filename
            (lambda (ok result-file)
              (if (not ok)
                  (progn
                    (message "[async-archive] Error: %s" result-file)
                    (async-installer-archive--finish-one))
                (async-installer-archive--batch-install
                 result-file
                 (lambda (p _e)
                   (if (and (eq (process-status p) 'exit)
                            (zerop (process-exit-status p)))
                       (message "[async-archive] Installed: %s" result-file)
                     (message "[async-archive] Install error: %s" result-file))
                   (async-installer-archive--finish-one)))))))
       (message "[async-archive] Download failed: %s" url)
       (async-installer-archive--finish-one)))))

(defun async-installer-archive--finish-one ()
  "Decrement running count and schedule next."
  (setq async-installer-archive--running
        (max 0 (1- async-installer-archive--running)))
  (async-installer-archive--schedule-next))

(defun async-installer-archive--schedule-next ()
  "Start next download if concurrency allows."
  (while (and async-installer-archive--queue
              (< async-installer-archive--running
                 async-installer-archive-max-concurrent))
    (let ((url (pop async-installer-archive--queue)))
      (setq async-installer-archive--running
            (1+ async-installer-archive--running))
      (async-installer-archive--process-url url))))

;;;###autoload
(defun async-installer-archive-start ()
  "Start downloading and installing all URLs in `async-installer-archive-list'."
  (interactive)
  (setq async-installer-archive--queue
        (copy-sequence async-installer-archive-list))
  (setq async-installer-archive--running 0)
  (message "[async-archive] Starting: %d items, concurrency=%d"
           (length async-installer-archive--queue)
           async-installer-archive-max-concurrent)
  (async-installer-archive--schedule-next))

;; ============================================================
;; Git async install/update (GitHub, GitLab, Codeberg, etc.)
;; ============================================================

(defvar async-installer-git-list '()
  "List of Git packages to install.
Each entry is a plist with keys:
  :repo      (required) Git repository URL (any host)
  :commit    Checkout target (SHA1)
  :branch    Branch name
  :tag       Tag name
  :subdir    Subdirectory to add to `load-path'
  :main      File to `load' after install
  :pre-build List of shell commands to run after checkout")

(defvar async-installer-git-install-dir
  (expand-file-name "external-packages" user-emacs-directory)
  "Directory where Git packages are cloned.")

(defvar async-installer-git-native-compile t
  "Non-nil means native-compile packages after install/update.")

;;;###autoload
(defun async-installer-git-add (repo &rest options)
  "Add a Git REPO with OPTIONS to the install list.
REPO can be any Git URL (GitHub, GitLab, Codeberg, self-hosted, etc.).
OPTIONS can include :branch, :tag, :commit, :subdir, :main, :pre-build."
  (let ((package (plist-put options :repo repo)))
    (setq async-installer-git-list
          (append async-installer-git-list (list package)))))

(defun async-installer-git--ensure-dir ()
  "Ensure `async-installer-git-install-dir' exists."
  (unless (file-directory-p async-installer-git-install-dir)
    (make-directory async-installer-git-install-dir t)))

(defun async-installer-git--native-compile (dir)
  "Native-compile DIR if available and enabled."
  (when (and async-installer-git-native-compile
             (fboundp 'native-compile-async))
    (message "[async-git] native-compile queued: %s" dir)
    (native-compile-async dir 2 t)))

(defun async-installer-git--make-clone-script (pkg-dir repo-url checkout-target pre-build)
  "Generate a shell/batch script to clone REPO-URL into PKG-DIR.
Checkout CHECKOUT-TARGET and run PRE-BUILD commands if any."
  (let ((git-exe (executable-find "git")))
    (unless git-exe
      (error "Git not found in PATH"))
    (when (stringp pre-build)
      (setq pre-build (list pre-build)))
    (if (eq system-type 'windows-nt)
        ;; Windows (cmd.exe)
        (let* ((git-exe-win (convert-standard-filename git-exe))
               (pkg-dir-win (convert-standard-filename pkg-dir))
               (post-lines
                (if pre-build
                    (mapconcat
                     (lambda (cmd)
                       (concat "  echo [DEBUG] Running: " cmd "\r\n"
                               "  " cmd "\r\n"
                               "  if errorlevel 1 exit /b 1\r\n"))
                     pre-build "")
                  "")))
          (format
           (concat
            "@echo off\r\n"
            "if not exist \"%s\" (\r\n"
            "  mkdir \"%s\"\r\n"
            "  if errorlevel 1 exit /b 1\r\n"
            "  \"%s\" clone \"%s\" \"%s\"\r\n"
            "  if errorlevel 1 exit /b 1\r\n"
            "  cd /d \"%s\"\r\n"
            "  \"%s\" checkout \"%s\"\r\n"
            "  if errorlevel 1 exit /b 1\r\n"
            "%s"
            ")\r\n"
            "exit /b 0\r\n")
           pkg-dir-win pkg-dir-win
           git-exe-win repo-url pkg-dir-win
           pkg-dir-win git-exe-win checkout-target
           post-lines))
      ;; Unix (bash/sh)
      (let* ((git-exe-sh (shell-quote-argument git-exe))
             (post-lines
              (if pre-build
                  (mapconcat
                   (lambda (cmd)
                     (format "echo '[DEBUG] Running: %s'\n%s\nif [ $? -ne 0 ]; then exit 1; fi\n"
                             cmd cmd))
                   pre-build "")
                "")))
        (format
         "set -e\nset -x\nif [ ! -d '%s' ]; then\n  mkdir -p '%s'\n  %s clone %s '%s'\n  cd '%s'\n  %s checkout %s\n  %s\nfi\nexit 0\n"
         pkg-dir pkg-dir
         git-exe-sh repo-url pkg-dir
         pkg-dir git-exe-sh checkout-target
         post-lines)))))

(defun async-installer-git--postprocess (package pkg-dir)
  "Handle :subdir, :main, and native compilation for PACKAGE at PKG-DIR."
  (when (plist-get package :files)
    (message "[async-git] WARNING: :files not supported (ignoring)"))
  (let ((subdir (plist-get package :subdir))
        (main-file (plist-get package :main)))
    (if subdir
        (let ((subdir-full (expand-file-name subdir pkg-dir)))
          (if (file-directory-p subdir-full)
              (add-to-list 'load-path subdir-full)
            (message "[async-git] WARNING: :subdir '%s' not found" subdir))
          (when main-file
            (let ((main-path (expand-file-name main-file subdir-full)))
              (if (file-readable-p main-path)
                  (condition-case err (load main-path)
                    (error (message "[async-git] WARNING: load failed: %s — %s"
                                    main-file err)))
                (message "[async-git] WARNING: main not found: %s" main-file)))))
      (add-to-list 'load-path pkg-dir)
      (when main-file
        (let ((main-path (expand-file-name main-file pkg-dir)))
          (if (file-readable-p main-path)
              (condition-case err (load main-path)
                (error (message "[async-git] WARNING: load failed: %s — %s"
                                main-file err)))
            (message "[async-git] WARNING: main not found: %s" main-file))))))
  (async-installer-git--native-compile pkg-dir)
  ;; Check for missing dependencies
  (let ((pkg-name (file-name-nondirectory (directory-file-name pkg-dir))))
    (async-installer-git--check-deps pkg-dir pkg-name)))

(defun async-installer-git--install-one (package callback)
  "Clone PACKAGE asynchronously, call CALLBACK with dir or nil."
  (async-installer-git--ensure-dir)
  (let* ((repo-url (plist-get package :repo))
         (checkout-target (or (plist-get package :commit)
                              (plist-get package :branch)
                              (plist-get package :tag)
                              "main"))
         (pre-build (plist-get package :pre-build))
         (pkg-name (file-name-nondirectory
                    (string-remove-suffix ".git" repo-url)))
         (pkg-dir (expand-file-name pkg-name async-installer-git-install-dir))
         (script-str (async-installer-git--make-clone-script
                      pkg-dir repo-url checkout-target pre-build)))
    (if (file-directory-p pkg-dir)
        (progn
          (let ((inhibit-message t))
            (message "[async-git] Exists, skipping: %s" pkg-dir))
          (funcall callback pkg-dir))
      (message "[async-git] Cloning: %s" repo-url)
      (async-start
       `(lambda ()
          (require 'subr-x)
          (let ((script-file
                 (make-temp-file "git-clone-" nil
                                 ,(if (eq system-type 'windows-nt) ".bat" ".sh"))))
            (with-temp-file script-file
              (when (eq system-type 'windows-nt)
                (set-buffer-file-coding-system 'dos))
              (insert ,script-str))
            (shell-command-to-string
             (if (eq system-type 'windows-nt)
                 (concat "cmd /c \"" script-file "\" 2>&1")
               (concat "/bin/sh \"" script-file "\" 2>&1")))))
       (lambda (output)
         (message "[async-git] Clone result for %s:\n%s" repo-url output)
         (if (file-directory-p pkg-dir)
             (progn
               (async-installer-git--postprocess package pkg-dir)
               (funcall callback pkg-dir))
           (message "[async-git] Clone failed: %s" repo-url)
           (funcall callback nil)))))))

(defun async-installer-git--install-all (finish-func)
  "Install all packages in `async-installer-git-list' sequentially.
Call FINISH-FUNC with (success-count fail-count)."
  (let ((packages (copy-sequence async-installer-git-list))
        (success 0)
        (fail 0))
    (cl-labels
        ((step ()
           (if (null packages)
               (funcall finish-func success fail)
             (let ((pkg (pop packages)))
               (async-installer-git--install-one
                pkg
                (lambda (dir-or-nil)
                  (if dir-or-nil
                      (setq success (1+ success))
                    (setq fail (1+ fail)))
                  (step)))))))
      (step))))

;;;###autoload
(defun async-installer-git-install-all-interactive ()
  "Interactively install all Git packages."
  (interactive)
  (async-installer-git--install-all
   (lambda (ok ng)
     (message "[async-git] Done! success=%d, fail=%d" ok ng))))

;;;###autoload
(defun async-installer-git-install-all-and-exit ()
  "Batch entry point: install all Git packages, then exit Emacs."
  (interactive)
  (let ((done nil) (fail-count 0))
    (async-installer-git--install-all
     (lambda (ok ng)
       (message "[async-git] Done! success=%d, fail=%d" ok ng)
       (setq done t fail-count ng)))
    (when noninteractive
      (while (not done)
        (sleep-for 0.1)
        (accept-process-output nil 0 100))
      (kill-emacs (if (zerop fail-count) 0 1)))))

;; --- Update ---

(defun async-installer-git--update-one (package callback)
  "Update PACKAGE if cached target differs from desired.
Call CALLBACK with pkg-dir or nil."
  (let* ((repo-url (plist-get package :repo))
         (update-tgt (or (plist-get package :commit)
                         (plist-get package :branch)
                         (plist-get package :tag)
                         "main"))
         (pkg-name (file-name-nondirectory
                    (string-remove-suffix ".git" repo-url)))
         (pkg-dir (expand-file-name pkg-name async-installer-git-install-dir))
         (cache-file (expand-file-name ".gitcommit" pkg-dir)))
    (if (not (file-directory-p pkg-dir))
        (progn
          (message "[async-git] Not installed, skip update: %s" pkg-name)
          (funcall callback nil))
      (let ((cached (if (file-exists-p cache-file)
                        (with-temp-buffer
                          (insert-file-contents cache-file)
                          (string-trim (buffer-string)))
                      "")))
        (if (string= cached update-tgt)
            (progn
              (message "[async-git] Already at %s: %s" update-tgt pkg-name)
              (funcall callback pkg-dir))
          (message "[async-git] Updating %s: %s -> %s" pkg-name cached update-tgt)
          (async-start
           (lambda ()
             (let ((default-directory pkg-dir))
               (shell-command "git pull")
               (shell-command (format "git checkout %s" update-tgt))
               t))
           (lambda (result)
             (if result
                 (progn
                   (with-temp-file cache-file (insert update-tgt))
                   (message "[async-git] Updated: %s -> %s" pkg-name update-tgt)
                   (async-installer-git--native-compile pkg-dir)
                   (funcall callback pkg-dir))
               (message "[async-git] Update failed: %s" pkg-name)
               (funcall callback nil)))))))))

(defun async-installer-git--update-all (finish-func)
  "Update all packages in `async-installer-git-list'.
Call FINISH-FUNC with (success-count fail-count)."
  (let ((packages (copy-sequence async-installer-git-list))
        (success 0) (fail 0))
    (cl-labels
        ((step ()
           (if (null packages)
               (funcall finish-func success fail)
             (let ((pkg (pop packages)))
               (async-installer-git--update-one
                pkg
                (lambda (dir-or-nil)
                  (if dir-or-nil
                      (setq success (1+ success))
                    (setq fail (1+ fail)))
                  (step)))))))
      (step))))

;;;###autoload
(defun async-installer-git-update-all-interactive ()
  "Interactively update all Git packages."
  (interactive)
  (async-installer-git--update-all
   (lambda (ok ng)
     (message "[async-git] Update done! success=%d, fail=%d" ok ng))))

;;;###autoload
(defun async-installer-git-update-all-and-exit ()
  "Batch entry point: update all Git packages, then exit Emacs."
  (interactive)
  (let ((done nil) (fail-count 0))
    (async-installer-git--update-all
     (lambda (ok ng)
       (message "[async-git] Update done! success=%d, fail=%d" ok ng)
       (setq done t fail-count ng)))
    (when noninteractive
      (while (not done)
        (sleep-for 0.1)
        (accept-process-output nil 0 100))
      (kill-emacs (if (zerop fail-count) 0 1)))))

;; ============================================================
;; Sudo helper (Linux/WSL)
;; ============================================================

;;;###autoload
(defun async-installer-build-sudo-command (cmd &optional password-file)
  "Return a shell command that runs CMD via sudo non-interactively.
Uses PASSWORD-FILE (default ~/.sudo_password) for the askpass script."
  (let* ((pf (or password-file (expand-file-name "~/.sudo_password")))
         (password (if (file-readable-p pf)
                       (with-temp-buffer
                         (insert-file-contents pf)
                         (string-trim (buffer-string)))
                     (error "Password file %s not found" pf))))
    (format "askpass_file=$(mktemp /tmp/askpass.XXXXXX.sh) && \
printf '#!/bin/sh\\necho %s\\n' > \"$askpass_file\" && chmod +x \"$askpass_file\" && \
env SUDO_ASKPASS=\"$askpass_file\" sudo -A %s && rm -f \"$askpass_file\""
            (shell-quote-argument password) cmd)))

;;;###autoload
(defun async-installer-build-sudo-commands (cmd-list &optional password-file)
  "Convert CMD-LIST to sudo commands using `async-installer-build-sudo-command'."
  (mapcar (lambda (cmd)
            (async-installer-build-sudo-command cmd password-file))
          cmd-list))

;; ============================================================
;; Git dependency detection
;; ============================================================

(defun async-installer-git--parse-pkg-requires (pkg-dir)
  "Parse Package-Requires from .el files in PKG-DIR.
Return a list of (PACKAGE MIN-VERSION) pairs."
  (let ((requires nil))
    (dolist (file (directory-files pkg-dir t "\\.el\\'"))
      (when (and (file-regular-p file) (null requires))
        (with-temp-buffer
          (insert-file-contents file nil 0 2048)
          (goto-char (point-min))
          (when (re-search-forward
                 "^;; Package-Requires:\\s-*\\((.+)\\)" nil t)
            (condition-case nil
                (setq requires (read (match-string 1)))
              (error nil))))))
    requires))

(defun async-installer-git--check-deps (pkg-dir pkg-name)
  "Check dependencies for PKG-NAME installed at PKG-DIR.
Display missing dependencies in a buffer if any are found."
  (let* ((requires (async-installer-git--parse-pkg-requires pkg-dir))
         (missing nil))
    (dolist (req requires)
      (let ((pkg (if (listp req) (car req) req))
            (ver (if (and (listp req) (cadr req)) (cadr req) "0")))
        (unless (or (eq pkg 'emacs)
                    (package-installed-p pkg)
                    (locate-library (symbol-name pkg)))
          (push (cons pkg ver) missing))))
    (when missing
      (let ((buf (get-buffer-create "*Async Installer Dependencies*")))
        (with-current-buffer buf
          (goto-char (point-max))
          (insert (format "\n%s requires:\n" pkg-name))
          (dolist (dep (nreverse missing))
            (insert (format "  - %s (>= %s)\n" (car dep) (cdr dep))))
          (insert "\n"))
        (display-buffer buf)
        (message "[async-git] %s has %d missing dep(s) — see *Async Installer Dependencies*"
                 pkg-name (length missing))))))

(provide 'async-installer)
;;; async-installer.el ends here
