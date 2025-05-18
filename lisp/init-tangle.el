;;; init-tangle.el --- Tangle and load config.org efficiently  -*- lexical-binding: t; -*-

;;; Commentary:

;; This module tangles a source Org file to Emacs Lisp only when it has changed,
;; based on a checksum. It also supports optional native compilation.

;;; Code:

(defgroup init-tangle nil
  "Tangle and load Org config conditionally."
  :group 'init
  :prefix "init-tangle-")

(defvar init-tangle-source "~/.emacs.d/config.org"
  "The master Org config file that tangles and loads sub-configs.")

(defvar init-tangle-output "~/.emacs.d/cache/tangled/config.el"
  "The .el file generated from the tangled Org config.")

(defcustom init-tangle-native-compile t
  "Whether to native-compile the tangled output."
  :type 'boolean)

(defun init-tangle--compute-hash (file)
  "Return SHA1 hash of FILE."
  (with-temp-buffer
    (insert-file-contents-literally file)
    (secure-hash 'sha1 (current-buffer))))

(defun init-tangle--hash-path (file)
  "Return path to sidecar hash file for FILE."
  (concat file ".sha1"))

(defun init-tangle-config ()
  "Tangle Org config if changed, then load it."
  (require 'org)
  (require 'ob-tangle)
  (when (file-exists-p init-tangle-source)
    (let* ((hash-path (init-tangle--hash-path init-tangle-output))
           (new-hash (init-tangle--compute-hash init-tangle-source))
           (old-hash (when (file-exists-p hash-path)
                       (with-temp-buffer
                         (insert-file-contents hash-path)
                         (buffer-string)))))
      (when (not (string= old-hash new-hash))
        (message "[init-tangle] Tangling %s..." init-tangle-source)
        (org-babel-tangle-file init-tangle-source init-tangle-output "emacs-lisp")
        (with-temp-file hash-path (insert new-hash))
        (when (and init-tangle-native-compile (fboundp 'native-compile))
          (ignore-errors
            (native-compile init-tangle-output))))
      (load-file init-tangle-output))))

(provide 'init-tangle)
;;; init-tangle.el ends here
