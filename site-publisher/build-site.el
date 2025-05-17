;;; build-site.el --- Publish Org site + web styling assets

;; Publish org-mode files into GitHub Pages

;; This is a customized script for the Emacs editor that compiles into
;; HTML my notes in the org-mode (.org files) markup.  It is invoked
;; by a GitHub Actions CI pipeline process, each time I commit new
;; content to the main Git branch, to regenerate my website from my
;; notes, making it available at https://xanalogica.github.io/emacs.d/.

;; Build website from Org-mode source files

;;; Code:

;; Set a package installation directory to avoid conflicts

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install minimal packages  (omitted: reformatter, nix-mode)
(dolist (pkg '(htmlize color-theme-modern))
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'htmlize)
(require 'ox-publish)
;;;AI OMITTED (require 'font-lock)

;; Emacs batch mode can't get colors unless a theme is enabled
(load-theme 'greiner t t)
(enable-theme 'greiner)
(global-font-lock-mode t)

;; Disable validation noise and postamble
(setq org-html-validation-link nil
      org-html-head "<meta charset='utf-8' /><style>.linenr{color:#888;font-family:monospace;padding-right:1em;}</style>"
      org-html-postamble nil
      org-html-include-default-style nil
      org-html-include-scripts nil
      org-src-fontify-natively t  ; colorize in buffer not via CSS
      org-src-preserve-indentation nil  ; ensure consistent indentation
      org-src-tab-acts-natively t
      org-html-number-lines t           ; 👈 enable line numbers
      org-html-htmlize-output-type 'inline-css  ; embed styles in exported HTML
      org-html-htmlize-font-prefix "org-" ; clean class names
)

;; Set your identity
(setq user-full-name "Xanalogica")
(setq user-mail-address "xanalogica@gmail.com")

(require 'seq)
(require 'subr-x)

(defun xan/expand-include-src-directives ()
  "Expand all #+INCLUDE_SRC directives into full Org source blocks.

Directive form:
  #+INCLUDE_SRC: \"file.el\" [language] [keyword=\"value\" ...]

Defaults:
  • language = emacs-lisp  
  • tangle   = yes  
  • lineno   = yes  

Supported keywords:
  • caption=\"...\"  
  • tangle=\"yes\" | \"no\"  
  • lineno=\"yes\" | \"no\""
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^#\\+INCLUDE_SRC:[ \t]+\"\\([^\"]+\\)\"\\(.*\\)$"
            nil t)
      (let* ((start  (match-beginning 0))
             (end    (match-end   0))
             (file   (match-string 1))
             (argstr (string-trim (match-string 2)))
             (tokens (if (string-empty-p argstr)
                         nil
                       (split-string argstr "[ \t]+" t)))
             ;; 1) language = first token without '='
             (lang   "emacs-lisp")
             (rest   tokens))
        (when (and rest (not (string-match-p "=" (car rest))))
          (setq lang (pop rest)))
        ;; 2) parse keywords from rest
        (let* ((caption
                (when-let (tok (seq-find
                                (lambda (t) (string-match "^caption=\"\\([^\"]+\\)\"$" t))
                                rest))
                  (string-match "^caption=\"\\([^\"]+\\)\"$" tok)
                  (match-string 1 tok)))
               (tangle
                (or (when-let (tok (seq-find
                                    (lambda (t) (string-match "^tangle=\"\\(yes\\|no\\)\"$" t))
                                    rest))
                      (string-match "^tangle=\"\\(yes\\|no\\)\"$" tok)
                      (match-string 1 tok))
                    "yes"))
               (lineno
                (or (when-let (tok (seq-find
                                    (lambda (t) (string-match "^lineno=\"\\(yes\\|no\\)\"$" t))
                                    rest))
                      (string-match "^lineno=\"\\(yes\\|no\\)\"$" tok)
                      (match-string 1 tok))
                    "yes"))
               (header-args
                (string-join
                 (delq nil
                       (list
                        (format ":tangle %s" tangle)
                        (when (string= lineno "yes") ":number-lines")))
                 " "))
               (code
                (if (file-readable-p file)
                    (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string))
                  (format ";;; ERROR: Cannot read file \"%s\"" file)))
               (replacement
                (concat
                 (when caption (format "#+CAPTION: %s\n" caption))
                 (format "#+BEGIN_SRC %s%s\n%s#+END_SRC\n"
                         lang
                         (if (string-empty-p header-args)
                             ""
                           (concat " " header-args))
                         code))))
          ;; Replace the directive with our fully-formed block
          (goto-char start)
          (delete-region start end)
          (insert replacement))))))

;; Define paths
(let* ((site-root (expand-file-name "../" default-directory))  ;; .emacs.d/
       (output-dir (expand-file-name "site-publisher/public/" site-root))
       (source-org (expand-file-name "config.org" site-root))
       (output-html (expand-file-name "index.html" output-dir))
       (webstyling-src (expand-file-name "site-publisher/webstyling" site-root))
       (webstyling-out (expand-file-name "webstyling" output-dir)))

  (make-directory output-dir t)

  ;;;AT OMITTED (require 'ox-html)

  ;; Publish config.org → index.html
  (message "[build-site] Exporting config.org → index.html")
  (with-current-buffer (find-file-noselect source-org)
    (let ((default-directory (file-name-directory source-org)))
      (save-excursion
        (xan/expand-include-src-directives))
      ;; Save the expanded .org for debugging
      (let ((expanded-path (expand-file-name "expanded-config.org" output-dir)))
        (write-region (point-min) (point-max) expanded-path nil 'silent)
        (message "[build-site] 💾 Wrote expanded Org to: %s" expanded-path))
      ;; Export to HTML
      (org-export-to-file 'html output-html nil nil nil nil
                          '(:with-toc t
                            :section-numbers nil
                            :html-validation-link nil
                            :html-postamble nil
                            :html-head "<meta charset='utf-8' /><style>.linenr{color:#888;font-family:monospace;padding-right:1em;}</style>"))))
  (message "[build-site] ✅ config.org published to %s" output-html)

  (defun xan/publish-and-log-file (plist file pub-dir)
    "Copy FILE to PUB-DIR and log the action. Ignore _PLIST."
    (let ((target (expand-file-name (file-name-nondirectory file) pub-dir)))
      (make-directory pub-dir t)
      (copy-file file target t)
      (message "[webstyling] Copied: %s → %s" file target)))

  ;; Define webstyling project
  (setq org-publish-project-alist
        `(("webstyling"
           :base-directory ,webstyling-src
           :publishing-directory ,webstyling-out
           :recursive t
           :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|svg\\|woff\\|ico\\|cur\\|html\\|pdf"
           :publishing-function xan/publish-and-log-file)))

  ;; Publish webstyling
  (message "[build-site] Publishing assets from webstyling/")
  (org-publish-project "webstyling" t)

  (message "[build-site] ✅ All assets published to %s" output-dir))

(message "[build-site] 🟢 Build completed.")
