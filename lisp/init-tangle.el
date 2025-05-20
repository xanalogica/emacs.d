;;; init-tangle.el --- Robust line-by-line tangling -*- lexical-binding: t; -*-

(require 'subr-x) ;; string-trim
(require 'org)    ;; org-babel-tangle-file
(require 'ob-tangle)

(defvar init-tangle-cache-dir
  (expand-file-name "tangled/" xan/cache-dir)
  "Directory for tangled .el files and their SHA1 sidecars.")

(defvar init-tangle-logfile "/tmp/emacs-init.log"
  "Log file for init-tangle debug messages.")

(defun init-tangle--log (fmt &rest args)
  "Append a timestamped message to `init-tangle-logfile`."
  (with-temp-file init-tangle-logfile
    (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
    (insert (apply #'format fmt args) "\n")))

(defun init-tangle--ensure-cache ()
  "Create `init-tangle-cache-dir` if needed and add it to `load-path`."
  (init-tangle--log "ensure-cache %s" init-tangle-cache-dir)
  (unless (file-directory-p init-tangle-cache-dir)
    (make-directory init-tangle-cache-dir t))
  (add-to-list 'load-path init-tangle-cache-dir))

(defun init-tangle--sha1 (file)
  "Return SHA1 of FILE’s contents, or nil if unreadable."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents-literally file)
      (secure-hash 'sha1 (current-buffer)))))

(defun init-tangle--tangle-phase (path)
  "Tangle a single Org PATH (relative to `user-emacs-directory`).
Caches to `init-tangle-cache-dir` with a SHA1 sidecar.  Returns BASE."
  (let* ((org-path  (expand-file-name path user-emacs-directory))
         (base      (file-name-base path))
         (el-path   (expand-file-name (concat base ".el") init-tangle-cache-dir))
         (hash-path (concat el-path ".sha1"))
         (new-hash  (init-tangle--sha1 org-path))
         (old-hash  (when (file-exists-p hash-path)
                      (string-trim
                       (with-temp-buffer
                         (insert-file-contents hash-path)
                         (buffer-string))))))
    (init-tangle--log "phase %s old=%s new=%s" path old-hash new-hash)
    (when (not (string= old-hash new-hash))
      (init-tangle--log "tangling %s → %s" path el-path)
      (condition-case err
          (progn
            (org-babel-tangle-file org-path el-path "\\`emacs-lisp\\'")
            (with-temp-file hash-path (insert (or new-hash "")))
            (when (fboundp 'native-compile)
              (ignore-errors (native-compile el-path)))
            (init-tangle--log "tangled %s" path))
        (error
         (init-tangle--log "ERROR tangling %s: %S" path err))))
    base))

(defun init-tangle--expand-includes (infile outfile)
  "Read INFILE line-by-line, handle all #+INCLUDE_SRC, and write to OUTFILE."
  (init-tangle--log "expand-includes %s → %s" infile outfile)
  (with-temp-buffer
    (insert-file-contents infile)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties
                   (point) (line-end-position))))
        (if (string-prefix-p "#+INCLUDE_SRC:" line)
            (let* ((fields (split-string line "[ \t]+" t))
                   (path   (string-trim (nth 1 fields) "\"" "\""))
                   (args   (mapconcat #'identity (nthcdr 2 fields) " "))
                   (tangle (when (string-match "tangle=\"\\(yes\\|no\\)\"" args)
                             (match-string 1 args))))
              (cond
               ((string= tangle "yes")
                (let ((base (init-tangle--tangle-phase path)))
                  (delete-region (point) (1+ (line-end-position)))
                  (insert
                   "#+BEGIN_SRC emacs-lisp\n"
                   "(message \"[tangle] Loading " base ".el\")\n"
                   "(require '" base ")\n"
                   "(message \"[tangle] Loading " base ".el...done\")\n"
                   "#+END_SRC\n")))
               ((string= tangle "no")
                (delete-region (point) (1+ (line-end-position))))
               (t
                (forward-line 1))))
          (forward-line 1)))
    (write-region (point-min) (point-max) outfile)
    (init-tangle--log "wrote expanded org %s" outfile))))

(defun init-tangle--final-tangle (expanded-org final-el)
  "Tangle all emacs-lisp src blocks from EXPANDED-ORG into FINAL-EL."
  (init-tangle--log "final-tangle %s → %s" expanded-org final-el)
  (condition-case err
      (progn
        (org-babel-tangle-file expanded-org final-el "\\`emacs-lisp\\'")
        (when (fboundp 'native-compile)
          (ignore-errors (native-compile final-el)))
        (init-tangle--log "final tangle done"))
    (error
     (init-tangle--log "ERROR final tangle: %S" err))))

;;;###autoload
(defun init-tangle-config-org-to-el ()
  "Expand includes, tangle phases, final tangle, then load config.el."
  (init-tangle--ensure-cache)
  (let* ((orgfile  (expand-file-name "config.org"    user-emacs-directory))
         (expanded (expand-file-name "config--expanded.org" init-tangle-cache-dir))
         (final-el (expand-file-name "config.el"           init-tangle-cache-dir)))
    (init-tangle--log "[START] %s" orgfile)
    (init-tangle--expand-includes orgfile expanded)
    (init-tangle--final-tangle   expanded final-el)
    (init-tangle--log "loading %s" final-el)
    (load-file final-el)
    (init-tangle--log "[DONE] loaded %s" final-el)))

(provide 'init-tangle)
;;; init-tangle.el ends here
