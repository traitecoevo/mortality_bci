# Using docker

Goal here is run our mortality analysis in docker containers. Docker containers wrap up a piece of software in a complete filesystem that contains everything it needs to run: code, runtime, system tools, system libraries – anything you can install on a server. This guarantees that it will always run the same, regardless of the environment it is running in.

## Installing and setting up docker

First install docker. Docker can be installed on OSX, Windows and Linux. A comprehensive guide can be found [here](http://docs.docker.com/mac/started/).

Once installed, Docker can be opened either via `Docker Quickstart Terminal` found in the docker folder in the applications folder, or accessed directly via the terminal using `docker-machine` followed by Docker commands. The instructions below use `docker-machine` and the terminal.

**Note** Multiple docker containers can be made. Details on how to do this are available [here](https://docs.docker.com/installation/mac/).


## Building/retrieving the `traitecoevo/mortality_bci` Docker container
First install the following Rpackages:
```
install.packages(c("RcppRedis", "R6", "digest", "docopt"))
devtools::install_github("traitecoevo/dockertest", ref="18-docker-machine") # Remove ref once changes are merged
devtools::install_github(c("gaborcsardi/crayon", "ropensci/RedisAPI", "richfitz/storr","traitecoevo/rrqueue"))
```

Building the docker image requires a lot of memory because it must compile and install `rstan`. As such, the container require 6GB for compilation. Using the terminal and the Docker command `docker-machine` we create a new Docker container with 6GB of virtual memory called `mem6GB`

```
docker-machine create --virtualbox-memory "6000" --driver virtualbox mem6GB
```

(`--virtualbox-memory "6000"` sets how much virtual memory is available to the docker container.)


Connect to the docker container by entering the following into the terminal:
```
eval "$(docker-machine env mem6GB)"
```

We auto generate the Dockerfile using [dockertest](https://github.com/traitecoevo/dockertest); the main configuration file is `docker/dockertest.yml`, which is declarative rather than a list of instructions.


To build the `traitecoevo/mortality_bci` docker container from the terminal first move into the folder `mortality_bci/docker` and then run:

```
Rscript -e 'dockertest::build(machine="mem6GB")'
```

The build can take a while, so we have pushed a pre-built image to dockerhub, which you can retrieve via the terminal using:
```
docker pull traitecoevo/mortality_bci:latest
```
(**Note** To push a docker container run `docker login`, then ` docker push traitecoevo/mortality_bci`.)

Below we setup 'workers' and a 'controller' using this image, but the workers run `rrqueue_worker_tee` rather than an interactive R session.

Building the image will also clone the source into the folder `self`, but if it's out of date, it will refresh it


## Running the mortality analysis in docker containers

Pull (or build) the most recent copy of the `traitecoevo/mortality_bci` container (see previous section).

Then move your working directory from `mortality_bci/docker` to the parent directory `mortality_bci`. And if you're starting in a new terminal, don't forget to set path variables:

```
eval "$(docker-machine env mem6GB)"
```

We're now going to setup three different Docker containers.

First, we start a container running Redis - this will sit in the background and act as a database catching results:

```
docker run --name mortality_bci_redis -d redis
```

**Note** if you have previously started redis, you'll need to do the following:
```
docker stop mortality_bci_redis
docker rm mortality_bci_redis
docker run --name mortality_bci_redis -d redis
```

Second, we start a container we'll call 'controller' from which we can create and queue jobs _from_. First laucnh the container and start R:

```
docker run --link mortality_bci_redis:redis -v ${PWD}:/root/mortality_bci -it traitecoevo/mortality_bci:latest R
```

Then in R, add the jobs:

```r
library(rrqueue)
packages <- c("rstan")
sources <- c("R/model.R",
             "R/task_compiler.R",
             "R/stan_functions.R",
             "R/utils.R")
obj <- queue("rrq", redis_host="redis", packages=packages, sources=sources)

tasks <- tasks_growth(iter = 10) # Set to 20 for testing, set to 1000 for actual deployment
create_dirs(unique(dirname(tasks$filename)))
enqueue_bulk(tasks, model_compiler, obj)
```

Third, we create workers that ask for, and then undertake, jobs from the controller. We:
```
eval "$(docker-machine env mem6GB)"
docker run --link mortality_bci_redis:redis -v ${PWD}:/root/mortality_bci -t traitecoevo/mortality_bci:latest rrqueue_worker --redis-host redis rrq
```
If you want to run multiple workers, open up more terminal tabs/windows and run the above code. (Currently we can only run 2 workers because the jobs require a lot of memory, due to compiling C++ code and rstan being a memory hungry monster).

