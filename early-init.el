;;; early-init.el --- Early Initialization -*- lexical-binding: t; no-byte-compile: t-*-

;; Author: Jeffrey Rush
;; Keywords: Emacs configuration
;; Homepage: https://github.com/xanalogica/.emacs.d.git

;;; Commentary:
;; Emacs 29.1+ configuration.

;;; Code:


;;;; configure emacs internal processes

; **********************************************************************
;   Define Various Emacs Policies re Debugging and Analysis of Issues
; **********************************************************************

; Useful for debugging problems with Emacs on occasion.
(setq debug-on-error t)
(setq stack-trace-on-error t)

;; this makes garbage collection less frequent, which speeds up init by about 2 seconds.
(setq gc-cons-threshold 80000000)     ;; JEFF
;;(setq gc-cons-threshold 800000)  ;; original value

(setq
  gc-cons-threshold               most-positive-fixnum
  read-process-output-max         (* 1024 1024 4) ; 4mb
  inhibit-compacting-font-caches  t
  inhibit-startup-message         t
  message-log-max                 16384
  package-enable-at-startup       nil
  load-prefer-newer               noninteractive)

(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)  ;; so PgUp PgDn give a visual slide too.
;;; (native-comp-available-p)



;;;; gc

;;;; default frame

(setq-default
  default-frame-alist '((width . 170)
                        (height . 56)
                        (tool-bar-lines . 0)
                        (bottom-divider-width . 0)
                        (right-divider-width . 1))

  initial-frame-alist           default-frame-alist
  frame-inhibit-implied-resize  t
  x-gtk-resize-child-frames     'resize-mode
  fringe-indicator-alist        (assq-delete-all 'truncation fringe-indicator-alist))

;;;  (setq default-frame-alist                           JEFF
;;;    '(
;;;  ;--      (top              . 200) ; only for GUI
;;;  ;--      (left             . 400)
;;;  ;--      (width            . 80)
;;;  ;--      (height           . 40)
;;;;;NEEDED?        (cursor-color     . "white")
;;;;;NEEDED?        (cursor-type      . box)
;;;;;NEEDED?        (foreground-color . "black")
;;;;;NEEDED?        (background-color . "white")
;;;        )
;;;      )





(unless (or (daemonp) noninteractive)
  (let ((restore-file-name-handler-alist file-name-handler-alist))
    (setq-default file-name-handler-alist nil)
    (defun restore-file-handler-alist ()
      (setq file-name-handler-alist
            (delete-dups (append file-name-handler-alist
                                 restore-file-name-handler-alist)))))

  (add-hook 'emacs-startup-hook #'restore-file-handler-alist 101)

  (when (fboundp #'tool-bar-mode)
    (tool-bar-mode -1))

  (when (fboundp #'scroll-bar-mode)
    (scroll-bar-mode -1)))

(when (featurep 'native-compile)
  (defvar inhibit-automatic-native-compilation)
  (setq inhibit-automatic-native-compilation nil)
  (defvar native-comp-async-report-warnings-errors)
  (setq native-comp-async-report-warnings-errors 'silent))

;;; (defun edit-init-file ()
;;;   "Edit `user-init-file'.
;;; With prefix argument prompts to select a file from all Emacs Lisp
;;; in `user-emacs-directory'."
;;;   (interactive)
;;;   (if current-prefix-arg
;;;       (find-file
;;;        (expand-file-name
;;;         (completing-read
;;;         "file"
;;;          (directory-files user-emacs-directory nil "^[^.].*.el$"))
;;;         user-emacs-directory))
;;;     (find-file (expand-file-name "init.el" user-emacs-directory))))

; **********************************************************************
;    Declare Package Archives From Which I May Pull Add-Ons to Emacs
; **********************************************************************

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
;; form of Git checkouts, or that I've written, is kept.

;;;;;;;;;; (add-to-list 'load-path "~/.emacs.d/lisp/")

;; Find all installed packages along the load-path and *activate* them by
;; executing their <package>-autoloads.el files.  It tries to make sure to
;; activate only the *latest* version of a package in case there are several
;; versions found.

(package-initialize)

;;;;;;;;;; (when (not package-archive-contents)   ;; if cache of 'package-archives' sites is empty,
;;;;;;;;;;   (package-refresh-contents))          ;;   then download latest package descriptions

; **********************************************************************
;                   Bootstrap the "quelpa" package
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
  nil) ;; disable quelpa peeking into MELPA and installing things from source

; **********************************************************************
;                Bootstrap the "use-package" macro
;
; The "use-package" macro provides a tidy, performance-oriented way to
; isolate the configuration and loading of packages within my Emacs
; configuration.  "use-package" is NOT a package manager itself but it
; interfaces with package managers to work.
; **********************************************************************

;;;;;;;;;; (quelpa ;; install the quelpa extension to use-package
;;;;;;;;;;   '(quelpa-use-package
;;;;;;;;;;      :fetcher git
;;;;;;;;;;      :url "https://github.com/quelpa/quelpa-use-package.git"
;;;;;;;;;;    )
;;;;;;;;;; )
;;;;;;;;;; (require 'quelpa-use-package)

;; Install the "use-package-ensure" which pulls in the rest of "use-package",
;; and then default the :ensure flag to always require a package if it is
;; referenced by "use-package".

;;;;;;;;;; (require 'use-package-ensure)
;;;;;;;;;; (setq use-package-always-ensure t)








(provide 'early-init)
;;; early-init.el ends here
