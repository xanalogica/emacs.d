; -*-mode: Emacs-Lisp; auto-recompile:t; outline-minor-mode:t-*-

;;; Modern Emacs (for Jeff) uses package.el but then I apply the Pallet package
;;; to 'Caskify' my ~/.emacs.d directory and use modern ELPA and such package
;;; repositories.
;;;
;;; To install a new package, do the following:
;;;
;;;   M-x list-packages
;;;
;;; My Directory Structure:  (I SHOULD PUT IT AT github.com/xanalogica/emacs.d)
;;;                           https://github.com/redguardtoo/emacs.d
;;;                           https://github.com/purcell/emacs.d/blob/master/README.md
;;;
;;;   ~/.emacs.d/
;;;      README.org	;; documentation about my setup
;;;      init.el	;; my init file for Emacs (versus ~/.emacs)
;;;      backups/	;; backup copies of ANY edited files
;;;      elpa/		;; Package.el packages installed from public repos
;;;      lisp/		;; individual .el files I create or work on
;;;      snippets/	;;
;;;      ARCHIVE	;; items that are retired and someday can be deleted
;;;      dict/		;; definitions for use by auto-complete.el
;;;
;;; (consider a "presentation" settings file)
;;; (consider a "credentials" settings file)
;;;
;;; *** PACKAGE REPOSITORIES ***
;;;
;;;    - http://melpa.org/
;;;
;;; *** KEY ADD-ON PACKAGES ***
;;;
;;;    - Org
;;;    - yasnippet
;;;    - flymake
;;;
;;;    - smartparens (auto-insert matched parens)
;;;    - web-mode    (for editing HTML)
;;;    - magit       (everything about Git)
;;;    - git-gutter.el  (mark the VGS diff)
;;;
;;;    - add the Keysnail addon to Firefox to get Emacs keybindings
;;;
;;; *** Emacs Startup Sequence ***
;;;
;;; When Emacs is started, it tries to load a Lisp program from an init file,
;;; looking in the following places in order:
;;;
;;;    - ~/.emacs              (I don't use this file)
;;;    - ~/.emacs.el           (I don't use this file)
;;;    - ~/.emacs.d/init.el    (This is the start of my Emacs configuration)
;;;
;;; It is NOT recommendeed to byte-compile your init file as it does not speed up
;;; startup very much and often leads to problems when you forget to recompile the
;;; file.
;;;
;;; You can use the command-line switch '-q' to prevent loading your init file.
;;;
;;; There may also be the following special init files, along the load-path:
;;;
;;;    - default.el    if found in load-path (it does not exist, it is loaded -after- your init file)
;;;    - site-start.el if found in load-path it is always loaded
;;;
;;; *** WHERE TO FIND EMACS COMMUNITY RESOURCES ***
;;;
;;;    - http://emacs.stackexchange.com/
;;;    - http://planet.emacsen.org/			Best collection of Emacs-related blogs
;;;    - https://github.com/languages/Emacs%20Lisp	Search for latest Elisp code
;;;       (and then click "Watch" to get notified of bugs and fixes automatically!)
;;;    - http://www.emacswiki.org/emacs/		Community Wiki

; **********************************************************************
;   Define Various Emacs Policies re Debugging and Analysis of Issues
; **********************************************************************


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-startup-message t)

; Useful for debugging problems with Emacs on occasion.
; (setq debug-on-error t)
; (setq stack-trace-on-error t)
; (debug-on-entry 'integerp)

; **********************************************************************
;           Define Where My Personal Emacs Macros Are Kept
; **********************************************************************

(add-to-list 'load-path
    "~/.emacs.d/lisp/")

; **********************************************************************
;       Insure I Can Always Reload my ~/.emacs File with a Hotkey
; **********************************************************************

(defun reload() (interactive)
  "Reload ~/.emacs"

  ;;(persistent-session-save-alist-to-file)

  (if (file-exists-p "~/.emacs.d/init.el")
      (load-file "~/.emacs.d/init.el"))
)
(global-set-key [f6] 'reload)

; **********************************************************************
;                  Load the org-mode Support Structure
; **********************************************************************

(require 'org-install)
(require 'org)

; **********************************************************************
;      Load My Neatly Organized (via org-mode) Emacs Configuration
; **********************************************************************

(org-babel-load-file "~/.emacs.d/config.org")
