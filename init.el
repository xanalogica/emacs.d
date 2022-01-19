; -*-mode: Emacs-Lisp; auto-recompile:nil; outline-minor-mode:t-*-

;;; I put a minimal set of configurations here in the init.el file, and
;;; the rest into my config.org file where I can more easily make nodes
;;; abouto settings and organize the various fragments of emacs-lisp.

; **********************************************************************
;   Define Various Emacs Policies re Debugging and Analysis of Issues
; **********************************************************************

; Useful for debugging problems with Emacs on occasion.
(setq debug-on-error t)
(setq stack-trace-on-error t)

; **********************************************************************
;     List Package Archives From Which I May Pull Add-Ons to Emacs
; **********************************************************************

(require 'package)  ;; the Emacs package manager

(setq package-enable-at-startup nil)

;; For security reasons, use "https" on all archive URLs.  And I
;; want to try turning on a requirement that all packages are signed
;; but haven't yet.

(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
  '("melpa-stable" . "https://stable.melpa.org/packages/"))

(setq package-archive-priorities ;; define the order of preference
  '(("melpa-stable" . 20)  ;; most preferred
    ("gnu" . 10)
    ("melpa" . 0))         ;; least preferred
)

;; Define where various packages I wish to use, that come in
;; form of Git checkouts, or that I've written, is kept.

(add-to-list 'load-path
  "~/.emacs.d/lisp/")

;; Find all installed packages along the load-path and *activate* them by
;; executing their <package>-autoloads.el files.  It tries to make sure to
;; activate only the *latest* version of a package in case there are several
;; versions found.

(package-initialize)

(when (not package-archive-contents)   ;; if cache of 'package-archives' sites is empty,
  (package-refresh-contents))          ;;   then download latest package descriptions

; **********************************************************************
;                   Bootstrap the "quelpa" package
;
; Quelpa is a tool for building and installing Emacs Lisp packages
; on-the-fly and directly from source.  I use it only to install packages
; NOT in one of the above package archives, to install those things that
; exist only in less well known Git repositories or the odd .el file.
;
; https://github.com/quelpa/quelpa
; **********************************************************************

(unless (package-installed-p 'quelpa)  ;; install if NOT already installed
  (with-temp-buffer
    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)
  )
)

(setq quelpa-checkout-melpa-p
  nil) ;; disable quelpa peeking into MELPA and installing things from source

; **********************************************************************
;                Bootstrap the "use-package" macro
;
; The "use-package" macro provides a tidy, performance-oriented way to
; isolate the configuration and loading of packages within my Emacs
; configuration.  "use-package" is NOT a package manager itself but it
; interfaces with package managers to work.
; **********************************************************************

(quelpa ;; install the quelpa extension to use-package
  '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"
   )
)
(require 'quelpa-use-package)

;; install the "use-package-ensure" which pulls in the rest of "use-package",
;; and then default the :ensure flag to always require a package if it is
;; referenced by "use-package".

(require 'use-package-ensure)
(setq use-package-always-ensure t)

; **********************************************************************
;      Load the Rest of My Configuration from My config.org File
; **********************************************************************

(use-package org)
(org-babel-load-file "~/.emacs.d/config.org" nil)  ;; Tangle into .el and load
(put 'upcase-region 'disabled nil)
