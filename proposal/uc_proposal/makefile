all: phd_proposal.pdf

phd_proposal.pdf: phd_proposal.tex ../../references.bib
	latexmk -pdf -quiet phd_proposal.tex;
	make clean;
	make view
	
clean:
	rm -fv *.aux *.fls *.log *blx.bib *.xml *.blg *.bbl *.fdb_latexmk *.bcf *.out

view: phd_proposal.pdf
	evince phd_proposal.pdf &