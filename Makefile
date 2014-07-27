all: figs ms

ms: figs
	make -C ms

figs: output/dbh.figs.pdf, output/basal.figs.pdf

output/fig1.pdf: analysis.R
	Rscript $<
