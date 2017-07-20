(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("these"
                 "\\documentclass{book}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

(setq org-publish-project-alist
      '(
        ("3Article"
         :base-directory "~/Projects/Thesis/These/"
         :base-extension "org"
         :publishing-directory "~/Projects/Thesis/These/"
         :publishing-function org-latex-publish-to-pdf
         :select-tags     ("3Article")
         :title "Article 3"
         :include ("main.org")
         :exclude "\\.org$"
         )
        ))
