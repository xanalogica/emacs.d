#! /home/jrush/Projects/emacs-29/src/emacs -x
;; -*- mode: elisp; -*-

;; Publish org-mode files on GitLab pages

;; This is a customized script for the Emacs editor that compiles into HTML my
;; notes in the org-mode (.org files) markup.  It is invoked by a CI pipeline
;; process, defined in the file .gitlab-ci.yml, each time I commit new content
;; to the main Git branch, to regenerate my website from my notes, making it
;; available at https://techdocs.zeomega.org.

;;; package --- Build Org website

;;; Commentary:
;; Build website from Org-mode source files

;;; Code:

;; Set a package installation directory to avoid conflicts

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages needed for HTML export

(package-install 'htmlize)
(package-install 'reformatter)
(package-install 'nix-mode)
(package-install 'color-theme-modern)

(require 'htmlize)
(require 'ox-publish)
(require 'font-lock)

;; Using this library is a work-around to get color in HTML exports.
;; Otherwise Emacs in batch mode cannot get the correct faces

(load-theme 'greiner t t)
(enable-theme 'greiner)

;; Set some variables for the export

(global-font-lock-mode t)
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-include-default-style nil
      org-src-fontify-natively t)

;; Define the project to be published

(setq org-publish-project-alist
      (list
       (list "config.org"
	     :recursive nil
	     :base-directory "."
	     :publishing-directory "./public"
	     :base-extension "org"
	     :include '("config.org")
	     :publishing-function 'org-html-publish-to-html
	     :with-author t
	     :with-creator nil
	     :with-toc t
	     :section-numbers nil
	     :time-stamp-file nil)
      )
)

;; Generate site

(org-publish-all t)

(message "Build completed")

(provide 'build-site)
;;; build-site.el ends here
