;;; early-init.el --- Tweaks that must run *before* package.el  -*- lexical-binding: t; -*-

;; Author: Jeffrey Rush
;; Keywords: Emacs configuration
;; Homepage: https://github.com/xanalogica/emacs.d.git

;;; Commentary:

;; My Initialization Flow:
;;   1. early-init.el
;;   2. init.el
;;   3. config.org

;; The idea of a `early-init.el` file was introduced in Emacs 27.1 and is loaded BEFORE
;; the package system and GUI is initialized.  This is a great place for
;; *startup optimizing*, because only here can you *prevent* things from
;; loading, rather than turn them off after-the-fact.

;; NOTE: `early-init.el` is meant to disable features, NOT initialize systems.  Since
;; this file is read by Emacs before the GUI is initialized, before a package system
;; is established and before the network is fully up, do NOT include your customizations
;; that are best left in the normal `init.el` file.

;; Emacs 30.1+ configuration.  (my version of Emacs at the time of composing this file)

;;; Code:

;; Require Emacs 30+
(when (< emacs-major-version 30)
  (user-error "This config requires Emacs 30+ (you have %s)" emacs-version))

;; ----------------------------------------------------------------------
;; Prevent initialization of the Emacs built-in `package.el` system until we
;; get to the `init.el` file where use the `straight.el` package manage instead.
;;
;; Since `straight.el` requires networking, loading files, and setting up load
;; paths — this is too early to do reliably in `early-init.el` before Emacs
;; initializes its GUI, environment variables, and process subsystems.

(setq package-enable-at-startup nil
      package-quickstart        nil)

;; ----------------------------------------------------------------------
;; To speed up the Emacs startup process, temporarily loosen the garbage
;; collector behavior so it does not compact memory during initialization.
;; Add a hook function to restore sane garbage collector behavior after
;; Emacs startup is complete.

(defvar xan/file-name-handler-alist file-name-handler-alist)

(setq file-name-handler-alist  nil
      gc-cons-threshold        most-positive-fixnum
      gc-cons-percentage       0.6)

;; Restore sane GC & file‑handler settings after init
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024) ; 64 MB
                  gc-cons-percentage 0.1
                  file-name-handler-alist xan/file-name-handler-alist)))

;; ----------------------------------------------------------------------
;; Disable UI elements to prevent flickering during startup and to load faster.
;; We'll restore them later in the initialization process.  Also don't load
;; any custom.el settings that might disrupt initialization because everything
;; is not yet configured to accept it.

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable the fancy graphical splash screen in its own buffer,
;; typically named *GNU Emacs*.

;; That screen normally shows:
;;
;;   - Emacs logo
;;   - Keybinding cheatsheet
;;   - Link to the Emacs tutorial

(setq inhibit-startup-screen t)

;; Don't load any custom.el automatically by redirecting it to an empty temp file
(setq custom-file (make-temp-file "emacs-custom-"))

;; ----------------------------------------------------------------------
;; In order to declutter my ~/.emacs.d/ directory I've adopted a layout
;; of putting anything that is cache related under ~/.emacs.d/cache/ and
;; anything that is dynamically generated data under ~/.emacs.d/data/.
;;
;; Here we establish global variables for those locations, insure certain
;; basic subdirectories are present, and tell the .el native compiler to
;; cache its .eln files there.

(defvar xan/cache-dir (expand-file-name "cache/" user-emacs-directory))
(defvar xan/data-dir  (expand-file-name "data/"  user-emacs-directory))

;; make the minimal set of subdirectories under cache/
(dolist (sub '("auto-save/" "auto-save-list/" "backups/"))
  (make-directory (expand-file-name sub xan/cache-dir) t))

(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/#" xan/cache-dir)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" xan/cache-dir) t)))

;; Redirect compiled code under cache/eln-cache/
(when (boundp 'native-comp-eln-load-path)
  (startup-redirect-eln-cache (expand-file-name "eln-cache/" xan/cache-dir)))

;; ----------------------------------------------------------------------
;;   Define Various Emacs Policies re Debugging and Analysis of Issues

;; Useful for debugging problems with Emacs on occasion.
; (setq debug-on-error t)
; (setq stack-trace-on-error t)
; (debug-on-entry 'integerp)

;; Respect DEBUG envvar as an alternative to --debug-init, and to
;; make sure startup is sufficiently verbose from this point on.
(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))


(message "early-init.el done (%.2fs)"
         (float-time (time-subtract (current-time) before-init-time)))

(provide 'early-init)
;;; early-init.el ends here
