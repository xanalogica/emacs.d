;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("title" "#+TITLE: ${1:title}\n" "title"
                        (=
                         (current-column)
                         5)
                        nil nil "/home/jrush/.emacs.d/snippets/org-mode/title.yasnippet" nil nil)
                       ("sb" "#+NAME: ${1:name}\n#+BEGIN_SRC ${2:language}\n  $3\n#+END_SRC\n" "#+srcname:..#+begin_src...#+end_src"
                        (or
                         (=
                          (current-column)
                          2)
                         (=
                          (current-column)
                          0))
                        nil nil "/home/jrush/.emacs.d/snippets/org-mode/sourceblock.yasnippet" "C-c y s" nil)
                       ("who" "* ${0}\n:PROPERTIES:\n:TITLE: ???\n:REPORTS_TO: ???\n:EMAIL: ???\n:END:\n** His/Her Responsibilities\n\n" "people" nil nil nil "/home/jrush/.emacs.d/snippets/org-mode/people.yasnippet" "C-c y p" nil)
                       ("opt" "#+OPTIONS: ${0}\n" "options"
                        (or
                         (=
                          (current-column)
                          3)
                         (=
                          (current-column)
                          0))
                        nil nil "/home/jrush/.emacs.d/snippets/org-mode/options.yasnippet" "C-c y o" nil)
                       ("fig" "#+attr_latex: width=$1\\textwidth\n#+ATTR_HTML: width=\"$2%\"\n#+caption: $3\n`(org-insert-link '(4))`\n$0\n" "figure"
                        (or
                         (=
                          (current-column)
                          3)
                         (=
                          (current-column)
                          0))
                        nil nil "/home/jrush/.emacs.d/snippets/org-mode/figure.yasnippet" "C-c y f" nil)
                       ("email" "#+EMAIL: ${1:xanalogica@gmail.com}\n" "email"
                        (or
                         (=
                          (current-column)
                          5)
                         (=
                          (current-column)
                          0))
                        nil nil "/home/jrush/.emacs.d/snippets/org-mode/email.yasnippet" nil nil)
                       ("el" "#+NAME: $1\n#+BEGIN_SRC emacs-lisp\n$0\n#+END_SRC\n" "el (emacs-lisp)"
                        (=
                         (current-column)
                         2)
                        nil nil "/home/jrush/.emacs.d/snippets/org-mode/el.yasnippet" nil nil)
                       ("block" "#+BEGIN_$1 $2\n  $0\n#+END_$1\n" "#+begin_...#+end_"
                        (or
                         (=
                          (current-column)
                          5)
                         (=
                          (current-column)
                          0))
                        nil
                        ((yas/indent-line 'fixed)
                         (yas/wrap-around-region 'nil))
                        "/home/jrush/.emacs.d/snippets/org-mode/block.yasnippet" "C-c y b" nil)))


;;; Do not edit! File generated at Thu Jun 21 18:34:09 2018
