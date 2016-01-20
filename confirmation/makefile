all: phd_confirmation.pdf clean

phd_confirmation.pdf: phd_report.pdf driver_species_draft.pdf modular_coevolution_draft.pdf
	pdftk $^ cat output $@

phd_report.pdf: phd_report.tex ../references.bib figures
	pdflatex -interaction nonstopmode $<; \
	biber phd_report; \
	pdflatex -interaction nonstopmode $<
	
driver_species_draft.pdf: driver_species_draft.tex
	pdflatex -interaction nonstopmode $<; \
	biber driver_species_draft; \
	pdflatex -interaction nonstopmode $<
	
modular_coevolution_draft.pdf: modular_coevolution_draft.tex
	pdflatex -interaction nonstopmode $<; \
	bibtex modular_coevolution_draft; \
	pdflatex -interaction nonstopmode $<; \
	bibtex modular_coevolution_draft; \
	pdflatex -interaction nonstopmode $<	
	
figures: $(wildcard ./code/*.R)
	Rscript $(wildcard ./code/*.R)

clean:
	rm -fv *.aux *.fls *.log *blx.bib *.xml *.blg *.bbl *.fdb_latexmk *.bcf *.out
