## Little wrapper to build the container in another directory, plus a
## dirty trick to work around things not behaving while out of
## internet.
build_container <- function(path) {
  if (identical(Sys.getenv("SKIP_BUILD"), "true")) {
    name <- dockertest:::dockertest_names()[["test"]]
    list(name=name, id=dockertest:::docker_image_id(name))
  } else {
    owd <- setwd(dirname(path))
    on.exit(setwd(owd))
    dockertest::build()
  }
}
