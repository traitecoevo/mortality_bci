#!/usr/bin/env Rscript
library(whisker)

vals <- list(models=iteratelist(c(1:3), value="model"),
						 chains=iteratelist(c(1:3), value="chain"))
str <- whisker.render(readLines("remake_models.yml.whisker"), vals)
writeLines(str, "remake_models.yml")

