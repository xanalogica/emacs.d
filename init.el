; -*-mode: Emacs-Lisp; auto-recompile:nil; outline-minor-mode:t-*-

;;;;;;
;;; How My Emacs Configuration Fits into the Emacs Startup Sequence
                                        ;
;;; When Emacs is started, it tries to load a ELisp program from an init file,
;;; looking in the following places in this order:
;;;
;;;    - ~/.emacs              -- I don't use this file
;;;    - ~/.emacs.el           -- I don't use this file
;;;    - ~/.emacs.d/init.el    -- This is the *start of my Emacs configuration*
;;;
;;; In case of problems with init.el, you can use the command-line switch '-q'
;;; to prevent it being loaded or '--debug-init' to get a traceback of the
;;; first error encountered.
;;;
;;; TIP: It is NOT recommendeed to byte-compile your init file as it does not
;;; speed up startup very much and often leads to problems when you forget to
;;; recompile the file.

;;; From the init.el file, I enable the Cask package manager and the Org-Mode
;;; library and the rest of my configuration is specified in an .org file
;;; where I can more easily make notes about settings and organize the various
;;; fragments.

;;; My Directory Structure:
;;
;;   ~/.emacs.d/
;;      README.org		;; light introduction to my setup
;;      ARCHIVE/		;; modules that I retired using and will someday delete
;;      backups/		;; backup copies of ANY edited files
;;      init.el			;; my init file for Emacs (versus ~/.emacs)
;;      config.org		;; my Emacs configuration in org-mode 'literate' format
;;      config.el		;; auto-built from my config.org each time Emacs starts
;;      custom-settings.el	;; tiny custom settings auto-edited by Emacs itself
;       Cask			;; installed modules list read by Cask and maintained by Pallet
;;      .cask/
;;         25.3/		;; cache/collection of modules pulled down by package manager
;;      snippets/		;; my library of text fragments for use by the snippet library
;;      lisp/			;; individual .el files I create or work on
;;      ac-dict/		;; auto-complete dictionary for various modes/languages
;;
;;   ~/.cask/
;;      Cask			;; specification file of dependencies of the Cask pgm
;;      bin/
;;         cask			;; command-line tool for invoking Cask outside of Emacs
;;      cask.el			;; ELisp source of the Cask library

; **********************************************************************
;   Define Various Emacs Policies re Debugging and Analysis of Issues
; **********************************************************************

; Useful for debugging problems with Emacs on occasion.
(setq debug-on-error t)
(setq stack-trace-on-error t)
; (debug-on-entry 'integerp)


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-startup-message t)

; **********************************************************************
;  Minimum Setup Necessary Before Loading Configuration from .org File
; **********************************************************************

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(add-to-list 'load-path
    "~/.emacs.d/lisp/")  ;; Define Where My Personal Emacs Macros Are Kept

(require 'org)
(org-babel-load-file
    "~/.emacs.d/config.org" nil)  ;; Tangle into .el and load
