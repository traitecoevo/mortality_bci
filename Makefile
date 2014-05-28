all: figs ms

ms: figs
	make -C ms

figs: output/fig1.pdf

output/fig1.pdf: analysis.R
	Rscript $<
