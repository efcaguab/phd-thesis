all: phd_proposal.pdf

phd_proposal.pdf: phd_proposal.tex ../../references.bib figures/*
	latexmk -pdf -quiet $<;
	make view;
	make clean
	
figures/dynamics.pdf: code/dynamic_model.R
	Rscript $<
	
figures/networks.pdf: code/networks.R
	Rscript $<

figures/hypo_c1.pdf: code/hypo_c1.R
	Rscript $<
	
figures/diagram_c1.pdf: code/diagram_c1.gv
	dot -Tpdf $< -x -o $@ 

clean:
	rm -fv *.aux *.fls *.log *blx.bib *.xml *.blg *.bbl *.fdb_latexmk *.bcf *.out

view: phd_proposal.pdf
	evince $< &
