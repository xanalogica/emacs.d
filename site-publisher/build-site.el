;; Publish org-mode files into GitHub pages

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

(setq user-full-name "Xanalogica")
(setq user-mail-address "xanalogica@gmail.com")

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

(defvar site-attachments (regexp-opt '("jpg" "jpeg" "gif" "png" "svg"
                                       "ico" "cur" "css" "js" "woff" "html" "pdf")))

;; Define the project to be published

(let* ((site-root (expand-file-name "../" default-directory)) ;; emacs.d/
       (output-dir (expand-file-name "site-publisher/public/" site-root))
       (source-file (expand-file-name "config.org" site-root))
       (output-file (expand-file-name "index.html" output-dir)))

  ;; Ensure output directory exists
  (make-directory output-dir t)

  (require 'ox-html)

  (message "[build-site] Publishing %s → %s" source-file output-file)

  ;; Publish config.org directly to index.html
  (with-current-buffer (find-file-noselect source-file)
    (let ((default-directory (file-name-directory source-file)))
      (org-export-to-file 'html output-file nil nil nil nil
                          '(:with-toc t
                            :section-numbers nil
                            :html-validation-link nil
                            :html-postamble nil
                            :html-head "<meta charset='utf-8' />"))))

;;  (with-current-buffer (find-file-noselect source-file)
;;    (org-export-to-file 'html output-file nil nil nil nil
;;                        `(:with-toc t
;;                          :section-numbers nil
;;                          :html-validation-link nil
;;                          :html-postamble nil
;;                          :html-head "<meta charset='utf-8' />")))

  (message "[build-site] ✅ Done")
)









;; (let* ((site-root (expand-file-name "../" default-directory)) ;; emacs.d/
;;        (output-dir (expand-file-name "site-publisher/public/" site-root))
;;        (source-file (expand-file-name "config.org" site-root)))
;; 
;;   ;; Make sure output directory exists
;;   (make-directory output-dir t)
;; 
;;   ;; Define a safe publishing function
;;   (defun my/org-publish-config-as-index (plist filename pub-dir)
;;     "Publish config.org to index.html inside pub-dir."
;;     (let ((output-file (expand-file-name "index.html" pub-dir)))
;;       (message "[build-site] Writing to: %s" output-file)
;;       (org-publish-org-to 'html filename output-file plist)))
;; 
;;   ;; Org-publish project alist
;;   (setq org-publish-project-alist
;;         `(("config-as-index"
;;            :base-directory ,site-root
;;            :publishing-directory ,output-dir
;;            :base-extension "org"
;;            :include (,source-file)
;;            :recursive nil
;;            :with-toc t
;;            :time-stamp-file nil
;;            :publishing-function my/org-publish-config-as-index)))
;; 
;;   ;; Do the publish
;;   (message "[build-site] Publishing %s → %s" source-file output-dir)
;;   (org-publish-project "config-as-index" t)
;;   (message "[build-site] ✅ Done")
;; )


;;; filename:    /home/runner/work/emacs.d/emacs.d/site-publisher/config.org
;;; output-file: /home/runner/work/emacs.d/emacs.d/site-publisher/public/index.html
;;; plist: (
;;;   :base-extension "org"
;;;   :base-directory "/home/runner/work/emacs.d/emacs.d/site-publisher/"
;;;   :publishing-directory "/home/runner/work/emacs.d/emacs.d/site-publisher/public/"
;;;   :recursive nil
;;;   :with-toc t
;;;   :time-stamp-file nil
;;;   :publishing-function my/org-publish-config-as-index
;;;   :exclude ".*"
;;;   :include ("config.org")
;;;   )




(setq org-publish-project-alist
  '(
;;;      ;; ----------------------------------------------------------------------
;;;      ;; Publish config.org as index.html Publishing Directory
;;;      ;; ----------------------------------------------------------------------
;;; 
;;;      ("config-as-index"
;;;       :base-extension "org"
;;;       :base-directory "."
;;;       :publishing-directory "../public/"
;;; 
;;;       ;; :with-author t
;;;       ;; :with-creator nil
;;;       :with-toc t
;;;       ;; :section-numbers nil
;;;       :time-stamp-file  nil
;;; 
;;;       :recursive nil
;;;       :publishing-function my/org-publish-config-as-index
;;;       :exclude ".*"                    ;; exclude everything...
;;;       :include ("config.org")          ;; ...except this one
;;;      )

     ;; ----------------------------------------------------------------------
     ;; Copy Diagrams (and Folders) into Publishing Directory
     ;; ----------------------------------------------------------------------

     ("webstyling"
      :base-directory             "webstyling"
      :publishing-directory       "./public/webstyling"
      :recursive                  t

      :base-extension             site-attachments

      :publishing-function        'org-publish-attachment
     )
  )
)

;; at time of publish, I'm in site-publisher/
;; and I'm writing to site-publisher/public


;; Generate site

;;; (let ((pub-dir (expand-file-name "./public/")))
;;;   (unless (file-directory-p pub-dir)
;;;     (make-directory pub-dir t)
;;;     (message "[publish] Writing to %s" (expand-file-name "index.html" pub-dir))
;;;   )
;;; )

(org-publish-all t)

(message "Build completed")

(provide 'build-site)
;;; build-site.el ends here
