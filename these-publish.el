(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("these"
                 "\\documentclass[11pt]{book}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

