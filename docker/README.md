# Using docker

Goal here is run our mortality analysis in docker containers. Docker containers wrap up a piece of software in a complete filesystem that contains everything it needs to run: code, runtime, system tools, system libraries – anything you can install on a server. This guarantees that it will always run the same, regardless of the environment it is running in.

## Installing and setting up docker

First install docker. Docker can be installed on OSX, Windows and Linux. A comprehensive guide can be found [here](http://docs.docker.com/mac/started/).

Once installed, Docker can be opened either via `Docker Quickstart Terminal` found in the docker folder in the applications folder, or accessed directly via the terminal using `docker-machine` followed by Docker commands. The instructions below use `docker-machine` and the terminal.

**Note** Multiple docker containers can be made. Details on how to do this are available [here](https://docs.docker.com/installation/mac/).


## Building/retrieving the traitecoevo/mortality_bci docker container
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
`--virtualbox-memory "6000"` sets how much virtual memory is available to the docker container.


Connect to the docker container by entering the following into the terminal:
```
eval "$(docker-machine env mem6GB)"
```

We auto generate the Dockerfile using [dockertest](https://github.com/traitecoevo/dockertest); the main configuration file is `docker/dockertest.yml`, which is declarative rather than a list of instructions. The file `rstan.yml` contains some additional instructions specifying a new rstan module for dockertest.


To build the `traitecoevo/mortality_bci` docker container from the terminal first move into the folder `mortality_bci/docker` and then run:
```
Rscript -e 'dockertest::build(machine="mem6GB")'
```
The build can take a while, so we have pushed a pre-built image to dockerhub, which you can retrieve via the terminal using:
```
docker pull traitecoevo/mortality_bci:latest
```
**Note** To push a docker container run `docker login`, then ` docker push traitecoevo/mortality_bci`.

Both the worker and the controller [NEED TO DEFINE WHAT THESE ARE] use this image, but the workers run `rrqueue_worker_tee` rather than an interactive R session.

Building the image will clone the source into the folder `self`, but if it's out of date, it will refresh it


## Running the mortality analysis in docker containers

Pull (or build) the most recent copy of the `traitecoevo/mortality_bci` container (see previous section).

Then move from `mortality_bci/docker` to the parent directory `mortality_bci`.

Now start a Docker container running Redis:

```
docker run --name mortality_bci_redis -d redis
```

**Note** if you have previously started redis do the following:
```
docker stop mortality_bci_redis
docker rm mortality_bci_redis
docker run --name mortality_bci_redis -d redis
```

Now we can start a controller container to que jobs _from_:
```
docker run --link mortality_bci_redis:redis -v ${PWD}:/root/mortality_bci -it traitecoevo/mortality_bci:latest R
```
This will open an R session where you can run prepare and queue jobs:

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

Now we create workers to that listen for, and undertake, jobs set by the controller container by openining up a new terminal tab or window in `mortality_bci` and running:
```
eval "$(docker-machine env mem6GB)" 
docker run --link mortality_bci_redis:redis -v ${PWD}:/root/mortality_bci -t traitecoevo/mortality_bci:latest rrqueue_worker --redis-host redis rrq
```
If you want to run multiple workers, open up more terminal tabs/windows and run the above code. (Currently we can only run 1 worker because of memory issues when compiling the models.)

## Running the analysis in multiple containers via docker-compose

In the previous section we fired up multiple containers in order to run our analysis. For that purpose we can also use [docker-compose](https://docs.docker.com/compose/). The file
`docker-compose.yml` contains a standard script specifying the types of containers we want
(`redis` and `worker`).  We can then start both the Redis and worker containers by
running:

```sh
docker-compose scale redis=1 worker=4
docker-compose up
```

Now, in another terminal, launch the controller (this part is the biggest pain)
```
eval "$(docker-machine env default)"
docker run --link docker_redis_1:redis -v ${PWD}/self:/root/mortality_bci -it traitecoevo/mortality_bci:latest R
```

Then, in this container (which is running `R`) test the system

First, can we reach Redis?

```r
RedisAPI::hiredis("redis")$PING() # should print "PONG"
```

Second, set up a simple queue:

```r
obj <- rrqueue::queue("rrq", redis_host="redis")
```

Run a trivial job:

```r
obj$enqueue(sin(1))
obj$task_result("1")
```

Run a mclapply like parallel map:

```r
res <- rrqueue::rrqlapply(1:30, sin, obj)
```

Cleanup.  Quit R (`q()`), then ctrl-C in the compose window.  The images can be removed with

```
docker-compose rm
```

Or, to stop just the workers without confirmation:

```
docker-compose rm --force worker
```
