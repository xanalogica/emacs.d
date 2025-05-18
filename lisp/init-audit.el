;;; init-audit.el --- Audit missing use-package targets  -*- lexical-binding: t; -*-

;;; Commentary:
;; This module checks `use-package` declarations in a given file
;; to see if the referenced packages are installed by straight.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun init-audit-use-package-missing (file)
  "Check which `use-package` declarations in FILE are not installed via straight.el."
  (interactive "fAudit use-package declarations in file: ")
  (let ((missing-packages '()))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^(use-package\\s-+\\([^ \n)]+\\)" nil t)
        (let ((pkg (intern (match-string 1))))
          (unless (or (featurep pkg)
                      (locate-library (symbol-name pkg))
                      (straight--installed-p pkg))
            (push pkg missing-packages)))))
    (if missing-packages
        (progn
          (message "[init-audit] Missing packages: %s"
                   (mapconcat #'symbol-name (cl-remove-duplicates missing-packages) ", "))
          (cl-remove-duplicates missing-packages))
      (message "[init-audit] All use-package targets are present.")
      nil)))

(provide 'init-audit)
;;; init-audit.el ends here
