.PHONY: main_clean

main_clean:
	latexmk main -C

criteo_pres.pdf: criteo_pres.tex
	latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f criteo_pres

