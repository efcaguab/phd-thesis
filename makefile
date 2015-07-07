all: literature

literature: literature.md
	pandoc -o literature.pdf --columns=2 --bibliography=references.bib literature.md;
	make view

view: literature.pdf
	evince literature.pdf &
