(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))


;; (setq org-latex-to-pdf-process
;;      '("pdflatex -interaction nonstopmode %b"
;;        "biber %b"
;;        "pdflatex -interaction nonstopmode %b"
;;        "pdflatex -interaction nonstopmode %b"))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("these"
                 "\\documentclass[12pt,a4paper,twoside]{ugathesis}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

