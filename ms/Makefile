TARGETS = MS.pdf
FIGURES_pdf = figs/fig1.pdf
FIGURES_png =  $(FIGURES_pdf:.pdf=.png)

all:  $(TARGETS) figs

figs: $(FIGURES_pdf) $(FIGURES_png)

MS.pdf: MS.md include.tex $(FIGURES) style.csl figs
	pandoc $< --csl=style.csl --filter pandoc-citeproc --bibliography=refs.bib --template=include.tex --variable mainfont="Times New Roman" --variable sansfont=Arial --variable fontsize=12pt --latex-engine=xelatex -o $@

style.csl:
	curl -k https://raw.githubusercontent.com/citation-style-language/styles/master/the-american-naturalist.csl > $@

figs/fig1.pdf: ../output/fig1.pdf
	mkdir -p figs
	cp $< $@

%.png: %.pdf
	convert -density 300 -background white -resize '1000' $< $@

clean:
	rm -f *.pdf figs/*