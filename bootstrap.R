#!/usr/bin/env Rscript
library(whisker)

# remake file
vals <- list(models=iteratelist(c(1:3), value="model"),
						 chains=iteratelist(c(1:3), value="chain"))
str <- whisker.render(readLines("remake_models.yml.whisker"), vals)
writeLines(str, "remake_models.yml")

# naked R file
vals <- list(models=iteratelist(c(1:3), value="model"),
						 chains=iteratelist(c(1:3), value="chain"),
						 effects=iteratelist(c("constant","trait","species","trait_species"), value = "effect"))
str <- whisker.render(readLines("run_all_models.R.whisker"), vals)
writeLines(str, "run_all_models.R")

