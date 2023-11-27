;;; init.el --- Main Configuration File -*- lexical-binding: t; no-byte-compile: t; outline-minor-mode:t -*-

;; Author: Jeffrey Rush
;; Keywords: Emacs configuration
;; Homepage: https://github.com/xanalogica/.emacs.d.git

;;; Commentary:
;; Emacs 29.1+ configuration.

;; and I put some stuff in early-init.el

;; I put a minimal set of configurations here in the init.el file, and
;; the rest into my config.org file where I can more easily make nodes
;; abouto settings and organize the various fragments of emacs-lisp.

;;; Code:

; **********************************************************************
;      Load the Rest of My Configuration from My config.org File
; **********************************************************************

(use-package org)

;; Insure the export backend ox-org is loaded so we can merge my config/*.org
;; pieces together and export the result into one common file.
(setq org-export-backends '(ascii html icalendar latex odt org))

;;; (let ()
 (find-file-read-only "~/.emacs.d/config.org")
 (org-export-to-file 'org "/tmp/config-exported.org")
 (kill-buffer)
;;; )

(org-babel-load-file "/tmp/config-exported.org" nil)  ;; Tangle into .el and load
;; (org-babel-load-file "~/.emacs.d/config.org" nil)  ;; Tangle into .el and load

(message "init.el finishing up")

;; Fake the footer to avoid warnings
;; (provide 'init)
;;; init.el ends here
