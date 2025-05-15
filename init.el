;; init.el --- Main Configuration File -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Jeffrey Rush
;; Keywords: Emacs configuration
;; Homepage: https://github.com/xanalogica/.emacs.d.git

;;; Commentary:

;; My Initialization Flow:
;;   1. early-init.el
;;   2. init.el
;;   3. config.org

;; After `early-init.el` completes its work, this file does the job of
;; setting many somewhat obscure Emacs settings and frame/window
;; settings, bootstrapping `straight.el` from the Internet and
;; declaring which package repos that `straight.el` should pull from.
;;
;; It then finishes by loading further configuration from a collection
;; of emacs-lisp source blocks tangled from config.org.  Using
;; config.org makes it easier for me to document my settings,
;; providing explanations and links to further learning material.

;; Emacs 30.1+ configuration.  (my version of Emacs at the time of composing this file)

;;; Code:

;; ----------------------------------------------------------------------
;; Set various somewhat obscure Emacs variables, for efficient operation.

(setq

  ;; I have some larger files and I don't want Emacs to complain about
  ;; them so I raised the threshold from 9MB.

  large-file-warning-threshold  20000000 ; 20MB

  ;; Prevent stale byte-code issues in scripts, while saving startup
  ;; I/O checks.  Especially relevant when not byte-compiling
  ;; everything, to prioritize non-byte-compiled source files to
  ;; prevent the use of stale byte-code.

  load-prefer-newer  'noninteractive

  ;; Maximum number of bytes to read from subprocess in a single chunk.
  ;;
  ;; Enlarge the value only if the subprocess generates very large
  ;; (megabytes) amounts of data in one go.  Improves LSP and
  ;; subprocess throughput.  Critical for lsp-mode, eglot, etc.

  read-process-output-max  (* 1024 1024 4) ; 4 MB

  ;; Do NOT compact (discard) cached fonts during garbage collection.
  ;;
  ;; Prevents GC-triggered font redisplay stutter with large fonts.
  ;; Very useful for multilingual documents or UI-heavy configs.

  ;; Some large fonts cause lots of consing and trigger GC.  If they
  ;; are discarded from the font caches during garbage collection,
  ;; they will need to be opened AGAIN during redisplay, which slows
  ;; down redisplay.
  ;;
  ;; If you see font-related delays in displaying some special
  ;; characters, and cannot switch to a smaller font for those
  ;; characters, set this variable non-nil.  Disabling compaction of
  ;; font caches might enlarge the Emacs memory footprint in sessions
  ;; that use lots of different fonts.

  inhibit-compacting-font-caches  t

  ;; Maximum number of lines to keep in the *Messages* log buffer.
  ;; Good practice that prevents runaway *Messages* buffer size while
  ;; still allowing debugging.

  message-log-max  16384

  ;; This disables loading of `site-start.el`, which some distros (like
  ;; Debian, Arch, etc.) use to inject site-wide behavior into all
  ;; Emacs sessions.  Use it in personal configs aiming for
  ;; reproducibility.

  site-run-file  nil
)  

;; ----------------------------------------------------------------------
;; Set various low-level UI presentation variables for maximum performance.

(setq

  ;; Prevents rounding errors during font/frame changes. Very useful.

  frame-resize-pixelwise  t

  ;; Resizing the Emacs frame can be a terribly expensive part of
  ;; changing the font.  By inhibiting this, we easily halve startup
  ;; times with fonts that are larger than the system default.
  ;;
  ;; Avoids expensive frame resizing during font/theme init. Good for
  ;; large fonts or tiling WMs (which I use under Pop_OS! by
  ;; System76).

  frame-inhibit-implied-resize  t

  ;; Prevents the message that says: "Welcome to GNU Emacs, one
  ;; component of the GNU operating system." that normally appears at
  ;; the top of the *scratch* buffer.

  inhibit-startup-message t

  ;; Frame customization
  default-frame-alist
    '((fullscreen . maximized)
      (menu-bar-lines . 0)
      (tool-bar-lines . 0)
      (vertical-scroll-bars . nil)
      (bottom-divider-width . 0)
      (right-divider-width . 1)
      ;; Uncomment to avoid theme flash:
      ;; (background-color . "#000000")
      ;; (ns-appearance . dark)
      ;; (ns-transparent-titlebar . t)
     )

  initial-frame-alist  default-frame-alist

  ;; annoyance supressions
  ;; byte-compile-warnings  '(not obsolete)
  ;; warning-suppress-log-types  '((comp) (bytecomp))
  ;; native-comp-async-report-warnings-errors  'silent

  ;; Silence message: "For information about GNU Emacs and the GNU system, type C-h C-a."
  ;; inhibit-startup-echo-area-message (user-login-name)
)

(setq-default

  ;; Avoid flicker or incorrect sizing of child frames
  ;; (e.g. completion popups) under GTK.  `resize-mode` ensures GTK
  ;; resizes them correctly without delays.  Especially helpful in
  ;; Wayland, where native resizing behavior differs from X11 and is
  ;; stricter.  It's a no-regret setting under GTK/Wayland.

  x-gtk-resize-child-frames  'resize-mode

  ;; Control which symbols appear in the left and right fringes of
  ;; Emacs windows to indicate things like:
  ;;
  ;;  - Buffer truncation
  ;;  - Continuation lines (soft-wrapped)
  ;;  - Buffer boundaries
  ;;  - Overlay arrows (e.g. for next-error)
  ;;  - Custom indicators (e.g. for git-gutter, flycheck, etc.)

  ;; fringe-indicator-alist  (assq-delete-all 'truncation fringe-indicator-alist)
)

;; Highly recommended under Wayland/PGTK.  Disables reading X resources
;; (e.g., .Xresources), which are irrelevant and possibly harmful
;; under Wayland.  Prevents color/font overrides that conflict with your
;; Emacs config.  Also disables xrdb-related hacks from old X11 setups.

(advice-add #'x-apply-session-resources :override #'ignore)

;; ----------------------------------------------------------------------
;; Bootstrap straight.el package system from the public Internet
;;   https://github.com/radian-software/straight.el

(defun my/online-p ()
  ;; test whether network is up before using it to download stuff
  (condition-case nil
      (ignore (getaddrinfo "github.com" "https" nil 'numeric))
    (error nil)))

(unless (my/online-p)
  (message "Emacs sees no network; skipping Straight recipe updates for now")
  (setq straight-check-for-modifications 'never))

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
  (load bootstrap-file nil 'nomessage))

;; ----------------------------------------------------------------------
;; Declare repos to pull from, and configure use-package to use straight.el

;; Define preferred archive sources
(setq straight-recipe-repositories
      '(gnu-elpa-mirror nongnu-elpa melpa))

;; NOTE: MELPA-stable is omitted from the above list as very few
;; people use MELPA‑stable with straight, because you can pin any
;; package to a commit instead.

;; Defer recipe refresh until idle (lowers startup cost)
(run-with-idle-timer 5 nil #'straight-pull-recipe-repositories)

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

;; ----------------------------------------------------------------------
;; Load the rest of my Emacs Configuration from my config.org file.

;; Load my tangle system
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(require 'init-tangle)
(require 'init-audit)

;;;; ;; Run my audit check Emacs is idle
;;;; (run-with-idle-timer
;;;;  10 nil
;;;;  (lambda ()
;;;;    (init-audit-use-package-missing (expand-file-name "lisp/" user-emacs-directory)
;;;; "~/.emacs.d/config.el")))  ;; variable: init-tangle-output

;; Ensure the export backend `ox-org' is loaded so we can merge my config/*.org
;; pieces together and export the result into one common file.
(setq org-export-backends '(ascii html icalendar latex odt org))

;; After initialization completes, tangle and load config.org.
(add-hook 'after-init-hook #'init-tangle-config)


(message "init.el done (%.2fs)"
         (float-time (time-subtract (current-time) before-init-time)))

;;; init.el ends here
