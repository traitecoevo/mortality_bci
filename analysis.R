#!/usr/bin/env Rscript

source("R/utils.R")

fig1 <- function(){
	plot(1:10, 1:10, type='b')
}

to.pdf(fig1(), "output/fig1.pdf", height=6, width=6)
