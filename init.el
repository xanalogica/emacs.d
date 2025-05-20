;; init.el --- Main Configuration File -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Jeffrey Rush
;; Keywords: Emacs configuration
;; Homepage: https://github.com/xanalogica/.emacs.d.git

;;; Commentary:

;; My Initialization Flow:
;;   1. load early-init.el
;;   2. load init.el
;;   3. tangle config.org into config.el and load
;;   4. tangle various NN-category-function.el files

;; Emacs 30.1+ configuration.  (my version of Emacs at the time of composing this file)

;;; Code:

;; ----------------------------------------------------------------------
;; Bootstrap straight.el package system from the public Internet
;;   https://github.com/radian-software/straight.el

(defun my/online-p ()
  "Test whether network is up before using it to download stuff."
  (condition-case _err
      (progn
        (url-retrieve-synchronously
         "https://github.com/"
         'silent 'inhibit-cookies)
        t)
    (error nil)))

(unless (my/online-p)
  (message "Emacs sees no network; skipping Straight recipe updates for now")
  (setq straight-check-for-modifications 'never))

(eval-when-compile  ;; to eliminate a warning about unknown func during compile
  (declare-function straight-pull-recipe-repositories "straight"))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)
)

;; ----------------------------------------------------------------------
;; Declare repos to pull from, and configure use-package to use straight.el

;; Define preferred archive sources
;; Replace the Savannah git remote for nongnu with the HTTP tarball mirror:
(setq straight-recipe-repositories
      '(gnu-elpa-mirror nongnu-elpa melpa))

;; NOTE: MELPA-stable is omitted from the above list as very few
;; people use MELPA‑stable with straight, because you can pin any
;; package to a commit instead.

(run-with-idle-timer
  5 nil
  (lambda ()
    (condition-case e
        (straight-pull-recipe-repositories)
      (error
        (message "[straight] recipe refresh failed: %S" e)))))

;; Make use-package rely on straight.el _by default_
(setq straight-use-package-by-default t)

;; Enable `use-package`'s :ensure, :init, :config etc.
(setq use-package-enable-imenu-support t)

;; Install use-package using straight.el, and make sure it's available to use in your config.
(straight-use-package 'use-package)

;; ----------------------------------------------------------------------
;; Install the org package early so I can write the rest of my Emacs
;; configuration in org-mode for easier documenting and then tangle
;; that configuration into .el files for compilation and loading into
;; Emacs.

(require 'org-macs)

(use-package org  ;; I WANT TO USE THE ORG PACKAGE SHIPPED WITH EMACS
  :straight (:type built-in)) ;; or omit this line to use org from Git

;; Ensure the export backend `ox-org' is loaded so we can merge my config/*.org
;; pieces together and export the result into one common file.
(setq org-export-backends '(ascii html icalendar latex odt org))

;; ----------------------------------------------------------------------
;; Load the rest of my Emacs Configuration from my config.org file.

;; Load my tangle system
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'init-tangle)

(init-tangle-config-org-to-el)

(message "init.el done (%.2fs)"
         (float-time (time-subtract (current-time) before-init-time)))

;;; init.el ends here
