all: phd_confirmation.pdf clean

phd_confirmation.pdf: phd_confirmation.tex ../references.bib ./figures/*
	pdflatex -interaction nonstopmode $<; \
	biber phd_confirmation; \
	pdflatex -interaction nonstopmode $<
	
./figures/*: ./code/*
	Rscript ./code/matching.R

clean:
	rm -fv *.aux *.fls *.log *blx.bib *.xml *.blg *.bbl *.fdb_latexmk *.bcf *.out
