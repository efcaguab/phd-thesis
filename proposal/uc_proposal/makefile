all: phd_proposal.pdf

phd_proposal.pdf: phd_proposal.tex ../../references.bib figures/*
	latexmk -pdf -quiet $<;
	make clean

figures/dynamics.pdf: code/dynamics.R
	Rscript $<

figures/networks.pdf: code/networks.R
	Rscript $<

figures/hypo_c1.pdf: code/hypo_c1.R
	Rscript $<

figures/critical_tran.pdf: code/critical_tran.R
	Rscript $<

figures/control_net.pdf: code/control_net.R
	Rscript $<

figures/schedule.pdf: code/schedule.R code/schedule.csv
	Rscript	$<

figures/diagram_c1.pdf: code/diagram_c1.gv
	dot -Tpdf $< -x -o $@

clean:
	rm -fv *.aux *.fls *.log *blx.bib *.xml *.blg *.bbl *.fdb_latexmk *.bcf *.out
