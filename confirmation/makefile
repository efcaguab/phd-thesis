all: phd_confirmation.pdf clean

phd_confirmation.pdf: phd_confirmation.tex ../references.bib ./figures/*.pdf
	pdflatex -interaction nonstopmode $<; \
	biber phd_confirmation; \
	pdflatex -interaction nonstopmode $<
	
./figures/*.pdf: ./code/*
	Rscript ./code/matching.R; \
	Rscript ./code/result_figures.R

clean:
	rm -fv *.aux *.fls *.log *blx.bib *.xml *.blg *.bbl *.fdb_latexmk *.bcf *.out
