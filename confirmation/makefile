all: phd_confirmation.pdf clean

phd_confirmation.pdf: phd_confirmation.tex ../references.bib
	latexmk -pdf -quiet $<;
	make clean
	
clean:
	rm -fv *.aux *.fls *.log *blx.bib *.xml *.blg *.bbl *.fdb_latexmk *.bcf *.out
