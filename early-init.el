;;; early-init.el --- Early Initialization -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Jeffrey Rush
;; Keywords: Emacs configuration
;; Homepage: https://github.com/xanalogica/emacs.d.git

;;; Commentary:

;; This early-init.el file was introduced in Emacs 27.1 and is loaded BEFORE
;; the package system and GUI is initialized.  This is a great place for
;; *startup optimizing*, because only here can you *prevent* things from
;; loading, rather than turn them off after-the-fact.

;; WARNING: It is NOT recommended that you move into early-init.el those
;; customizations that can be left in the normal init files, because
;; early-init.el is read before the GUI is initialized, so customizations
;; related to GUI features *will not work reliably* in early-init.el.

;; Emacs 29.1+ configuration.

;;; Code:

(cond ((version< emacs-version "29.1")
       (warn "My Emacs configuration requires Emacs 29.1 and above!"))
)

;; **********************************************************************
;;   Define Various Emacs Policies re Debugging and Analysis of Issues
;; **********************************************************************

;; Useful for debugging problems with Emacs on occasion.
; (setq debug-on-error t)
; (setq stack-trace-on-error t)
; (debug-on-entry 'integerp)

;; Respect DEBUG envvar as an alternative to --debug-init, and
;; to make are startup sufficiently verbose from this point on.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

;; **********************************************************************
;;                 Define Garbage Collection Policies
;;
;; Garbage collection is a big contributor to startup times.  This fends
;; it off, but will be reset later by `gcmh-mode'.  Not resetting it later
;; will cause stuttering/freezes.
;;
;; **********************************************************************

(setq
      ;; Defer garbage collection further back in the startup process, until
      ;; reset later by `gcmh-mode'.  NOT resetting it later will cause
      ;; stuttering and freezing during long-term interactive use.

      gc-cons-threshold                  most-positive-fixnum

      ;; Portion of the heap used for allocation.
      ;;
      ;; Garbage collection can happen automatically once this portion of the
      ;; heap has been given out since the last garbage collection.
      ;;
      ;; By binding this temporarily to a large number, you can effectively
      ;; prevent garbage collection during a part of the program.  But be sure
      ;; to get back to the normal value soon enough, to avoid system-wide
      ;; memory pressure, and never use a too-high value for prolonged periods
      ;; of time.

      gc-cons-percentage                 0.6

      ;; In noninteractive sessions only, prioritize non-byte-compiled source
      ;; files to prevent the use of stale byte-code.  Otherwise, it saves us
      ;; a little I/O time to skip the mtime checks on every *.elc file.

      load-prefer-newer                  'noninteractive
)

(setq
      site-run-file                      nil

      ;; Maximum number of bytes to read from subprocess in a single chunk.
      ;;
      ;; Enlarge the value only if the subprocess generates very large (megabytes)
      ;; amounts of data in one go.

      read-process-output-max            (* 1024 1024 4) ; 4 MB

      ;; Do NOT compact (discard) cached fonts during garbage collection.
      ;;
      ;; Some large fonts cause lots of consing and trigger GC.  If they are
      ;; discarded from the font caches during garbage collection, they will need
      ;; to be opened AGAIN during redisplay, which slows down redisplay.
      ;;
      ;; If you see font-related delays in displaying some special characters, and
      ;; cannot switch to a smaller font for those characters, set this variable
      ;; non-nil.  Disabling compaction of font caches might enlarge the Emacs
      ;; memory footprint in sessions that use lots of different fonts.

      inhibit-compacting-font-caches     t
)

;; **********************************************************************
;;    Declare Package Archives From Which I May Pull Add-Ons to Emacs
;; **********************************************************************

(setq

      ;; Package initialize occurs automatically, BEFORE user-init-file is
      ;; loaded, but AFTER early-init-file.  We handle package initialization,
      ;; so we must prevent Emacs from doing it early and let us do it!

      package-enable-at-startup          nil

      ;; `use-package' is built into Emacs since version 29 but not loaded
      ;; until we say so.  So before we load `use-package', we can enable imenu
      ;; support so it will see `use-package' declarations.

      use-package-enable-imenu-support   t
)

(require 'package)  ;; the Emacs package manager

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
;; form of Git checkouts, or that I've written, are kept.

;;;;; (add-to-list 'load-path "~/.emacs.d/lisp/")

;; Find all installed packages along the load-path and *activate* them by
;; executing their <package>-autoloads.el files.  It tries to make sure to
;; activate only the *latest* version of a package in case there are several
;; versions found.

(package-initialize)

;;;;;;;;;; (when (not package-archive-contents)   ;; if cache of 'package-archives' sites is empty,
;;;;;;;;;;   (package-refresh-contents))          ;;   then download latest package descriptions

; **********************************************************************
;                Bootstrap the "quelpa" Package Manager
;
; Quelpa is a tool for building and installing Emacs Lisp packages on-the-fly
; and directly from source.  I use it only to install packages NOT in one of
; the above package archives, to install those things that exist only in less
; well known Git repositories or the odd .el file around the Internet.
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
  nil)  ;; disable quelpa peeking into MELPA and installing things from source

; **********************************************************************
;                Bootstrap the "use-package" macro
;
; The `use-package' macro provides a tidy, performance-oriented way to isolate
; the configuration and loading of packages within my Emacs configuration.
; `use-package' is NOT a package manager itself but it interfaces with package
; managers to work.
; **********************************************************************

; adds support for the :quelpa keyword in use-package clauses.

(quelpa  ;; install the `quelpa' extension to `use-package'
  '(quelpa-use-package
     :fetcher git
     :url "https://github.com/quelpa/quelpa-use-package.git"
   )
)
(require 'quelpa-use-package)

;; Install the "use-package-ensure" which pulls in the rest of "use-package",
;; and then default the :ensure flag to always require a package if it is
;; referenced by "use-package".

;;;;;;;;;; (require 'use-package-ensure)
;;;;;;;;;; (setq use-package-always-ensure t)

; **********************************************************************
;                  Compile .el into .elc Bytecode
; **********************************************************************

; native-comp-deferred-compilation
(setq native-comp-jit-compilation nil)

(when (featurep 'native-compile)
  (defvar inhibit-automatic-native-compilation)
  (setq   inhibit-automatic-native-compilation nil)
  (defvar native-comp-async-report-warnings-errors)
  (setq   native-comp-async-report-warnings-errors 'silent))

; **********************************************************************
;      Prevent Some Brief UI Annoyances that Flash on the Screen
; **********************************************************************

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
;;
;; Disable unnecessary interfaces
;;
;; It will be faster to disable them here before they've been initialized.

;;;;; (setq-default
;;;;;   x-gtk-resize-child-frames     'resize-mode
;;;;;   fringe-indicator-alist        (assq-delete-all 'truncation fringe-indicator-alist)
;;;;; )

(setq

      ;; Avoid displaying the start screen.

      inhibit-startup-message            t

      ;; Disable built-in mode-line because we have own config

      mode-line-format                   nil

      ;; ???

      frame-resize-pixelwise             t

      ;; Resizing the Emacs frame can be a terribly expensive part of changing
      ;; the font.  By inhibiting this, we easily halve startup times with
      ;; fonts that are larger than the system default.

      frame-inhibit-implied-resize       t

      default-frame-alist '(
        (fullscreen . maximized)

        ;; You can turn off scroll bars by uncommenting these lines:
        ;; (vertical-scroll-bars . nil)
        ;; (horizontal-scroll-bars . nil)

        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)

        (bottom-divider-width . 0)
        (right-divider-width . 1)
        ;; Setting the face in here prevents
        ;; flashes of color as the theme gets activated
        (background-color . "#000000")
        (ns-appearance . dark)
        (ns-transparent-titlebar . t)
    )
    ;;; initial-frame-alist           default-frame-alist
)

(tool-bar-mode -1)  ; All these tools are in the menu-bar anyway
(menu-bar-mode -1)

; **********************************************************************
;      Message Log Buffer
; **********************************************************************

(setq

      ;; Maximum number of lines to keep in the message log buffer.

      message-log-max                    16384
)


;; annoyance supression
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(message "early-init.el finishing up")

(provide 'early-init)
;;; early-init.el ends here
