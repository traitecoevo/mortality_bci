## Little wrapper to build the container in another directory, plus a
## dirty trick to work around things not behaving while out of
## internet.
build_container <- function(path) {
  ## I need the treatment of "machine" to work differently within
  ## dockertest; only check if it is given otherwise use the machine
  ## that is running already?  At the moment, I have this in the
  ## mem3GB machine, because I don't have a mem6GB machine.  It would
  ## be better for, if machine was omitted here, us to just keep going
  ## if DOCKER_HOST is defined.  If it's _not_defined though we'll
  ## connect to the machine default, or the first in docker-machine
  ## ls?
  if (identical(Sys.getenv("SKIP_BUILD"), "true")) {
    name <- dockertest:::dockertest_names()[["test"]]
    list(name=name, id=dockertest:::docker_image_id(name, machine="mem3GB"))
  } else {
    owd <- setwd(dirname(path))
    on.exit(setwd(owd))
    dockertest::build(machine="mem3GB")
  }
}
