; -*-mode: Emacs-Lisp; auto-recompile:t; outline-minor-mode:t-*-

; Modern Emacs (for Jeff) uses package.el.  It is interactively invoked
; as:
;
;     ???

; **********************************************************************
;   Define Various Emacs Policies re Debugging and Analysis of Issues
; **********************************************************************

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
(add-to-list 'load-path
    "~/.emacs.d/plugins/")

; **********************************************************************
;                  Load the org-mode Support Structure
; **********************************************************************

(require 'org-install)
(require 'org)

; **********************************************************************
;       Insure I Can Always Reload my ~/.emacs File with a Hotkey
; **********************************************************************

(defun reload() (interactive)
  "Reload ~/.emacs"

  ;;(persistent-session-save-alist-to-file)

  (if (file-exists-p "~/.emacs")
      (load-file "~/.emacs"))
)
(global-set-key [f6] 'reload)

; **********************************************************************
;      Load My Neatly Organized (via org-mode) Emacs Configuration
; **********************************************************************

(org-babel-load-file "~/notes/emacs-config.org")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "3c1e3f7f6c9bbd7ee8e34162eea3ccb8ae58429f")
 '(case-fold-search t)
 '(current-language-environment "English")
 '(dired-dwim-target t)
 '(evernote-developer-token
   "S=s526:U=5256ba1:E=15778ae1715:C=15020fce8a8:P=1cd:A=en-devtoken:V=2:H=c6f6fd9d9ef16a43ed79a1a6fd79f153")
 '(evernote-ruby-command "/usr/bin/ruby20")
 '(evernote-username "jrush110")
 '(global-font-lock-mode t nil (font-lock))
 '(org-agenda-diary-file "~/notes/journal.org")
 '(org-agenda-files (quote ("~/Dropbox/Documents/Org-Mode/NotesOnline.org")))
 '(org-clock-idle-time 15)
 '(org-clock-into-drawer t)
 '(org-clock-mode-line-total (quote today))
 '(org-clock-persist t)
 '(org-deadline-warning-days 30)
 '(org-default-priority 67)
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "FEEDSTATUS" "LOGBOOK")))
 '(org-file-apps
   (quote
    ((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . "evince %s")
     ("\\.doc\\'" . "libreoffice %s"))))
 '(org-highest-priority 65)
 '(org-insert-mode-line-in-empty-file t)
 '(org-log-into-drawer t)
 '(org-log-refile (quote time))
 '(org-lowest-priority 69)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-use-outline-path (quote file))
 '(org-time-stamp-rounding-minutes (quote (15 15)))
 '(safe-local-variable-values
   (quote
    ((cryptkey . "jrush@taupro.com")
     (auto-recompile . t)
     (outline-minor-mode . t)
     (folded-file . t))))
 '(save-place t nil (saveplace))
 '(send-mail-function (quote smtpmail-send-it))
 '(transient-mark-mode t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(yas-use-menu (quote abbreviate)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
 '(mode-line ((((class color) (min-colors 88)) (:background "deep sky blue" :foreground "black" :box (:line-width -1 :style released-button) :height 1.3))))
 '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background light)) (:background "navajo white" :foreground "grey20" :box (:line-width -1 :color "grey75") :weight light)))))

;;; **********************************************************************
;;;                Define Various Emacs Behavior Policies
;;; **********************************************************************

;;;(setq special-display-buffer-names
;;;    '(
;;;      "*Completions*"
;;;      "*grep*"
;;;      "*tex-shell*"
;;;      "*Faces*")
;;;    )
