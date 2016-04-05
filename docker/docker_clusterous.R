#!/usr/bin/env Rscript

## Little test script for generating the pseudo-clusterous container.
## Not part of dockertest, but designed to work with it.
library(dockertest)

run <- function() {
  cfg <- dockertest:::yaml_read("clusterous_env.yaml")
  copy <- cfg$environment$copy
  cmd <- cfg$environment$components$worker$cmd
  image <- dockertest:::dockertest_names()
  image_cl <- paste0(image, "_clusterous")
  COPY <- paste(sprintf("COPY %s %s", copy, copy), collapse="\n")
  str <- sprintf('FROM %s\n%s\nCMD %s',
                 image, COPY, cmd)
  filename <- "clusterous.dock"
  writeLines(str, filename)

  ign <- ".dockerignore"
  old <- readLines(ign)
  on.exit(writeLines(old, ign))
  writeLines("results", ign)

  dockertest::docker_machine_init()
  args <- c("build", "-t", image_cl, "-f", filename, "..")
  message("Build container: ", image_cl)
  system2(callr::Sys_which("docker"), args)
  file.remove(filename)
}

run()
