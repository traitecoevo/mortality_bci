# Using docker

Goal here is run our mortality analysis in docker containers. Docker containers wrap up a piece of software in a complete filesystem that contains everything it needs to run: code, runtime, system tools, system libraries – anything you can install on a server. This guarantees that it will always run the same, regardless of the environment it is running in.

## Installing and setting up docker

First you'll need to install docker. Docker can be installed on OSX, Windows and Linux. A comprehensive guide can be found [here](http://docs.docker.com/mac/started/).

Once installed, Docker can be opened either via `Docker Quickstart Terminal` found in the docker folder in the applications folder, or accessed directly via the terminal using `docker-machine` followed by Docker commands.

Multiple docker containers can be made. Details on how to do this are available [here](https://docs.docker.com/installation/mac/).


## Building / retrieving the `traitecoevo/mortality_bci` docker container

Building this docker image takes a lot of memory, primarily because it must install `rstan`. As such, we allow 3GB for compilation.  Using terminal and the Docker command `docker-machine` we can create a new Docker container called `mem3GB`

```
docker-machine create --virtualbox-memory "3000" --driver virtualbox mem3GB
```
The ` --virtualbox-memory "3000"` just tells Docker to allow 3GB of memory for compilation.


To connect to this then run

```
eval "$(docker-machine env mem3GB)"
```

We auto generate the Dockerfile using [dockertest](https://github.com/traitecoevo/dockertest); the main configuration file is `docker/dockertest.yml`, which is declarative rather than a list of instructions. The file `rstan.yml` contains some additional instructions specifying a new rstan module for dockertest.

To build the image, open up a terminal window in the `docker` folder, make sure docker is running, and then run

```
Rscript -e 'dockertest::build()' *DOES NOT WORK AS RELIES ON BOOT2DOCKER which is now superceded by docker-machine*
```

This builds the image `traitecoevo/mortality_bci`. The build can take a while, so we have pushed a pre-built image to dockerhub, which you can retrieve  using

```
docker pull traitecoevo/mortality_bci:latest
```
(To push docker container run `docker login`, then ` docker push traitecoevo/mortality_bci`).

Both the worker and the controller use this image, but the workers run `rrqueue_worker_tee` rather than an interactive R session.

Building the image will clone the source into the folder `self`, but if it's out of date, this will refresh it

```sh
. ./helpers.sh  # Load helper functions
clone_or_pull ../ self
```


## Running the analysis in docker containers

Pull (or build) the most recent copy of the `traitecoevo/mortality_bci` image (see previous section).

Then move to parent directory `mortality_bci`:

```
cd ../
```

and start a container running Redis:

```
docker run --name mortality_bci_redis -d redis
```
(if you have previously started need to either delete container first using
`docker rm mortality_bci_redis`)

or ?????????

Open up a new terminal tab or window and navigate to `mortality_bci`

install rrqueue
```
install.packages(c("RcppRedis", "R6", "digest", "docopt"))
devtools::install_github(c("gaborcsardi/crayon", "ropensci/RedisAPI", "richfitz/RedisHeartbeat", "richfitz/storr")) # note I had an error with installing RedisHeartbeat
devtools::install_git("https://github.com/traitecoevo/rrqueue")
```

Then start some worker containers listening on queue `rrq` (for now not running in daemon mode, though not actually interactive either; we'll do this with compose or something else later)

```
docker run --link mortality_bci_redis:redis -v ${PWD}:/root/mortality_bci -t traitecoevo/mortality_bci:latest rrqueue_worker --redis-host redis rrq
```

Start a container to queue jobs _from_:

```
docker run --link mortality_bci_redis:redis -v ${PWD}:/root/mortality_bci -it traitecoevo/mortality_bci:latest R
```

Then within that session, start working, in R, create the queue controller:

```r
library(rrqueue)
packages <- c("rstan")
sources <- c("R/model.R",
             "R/task_compiler.R",
             "R/stan_functions.R",
             "R/utils.R")
obj <- queue("rrq", redis_host="redis", packages=packages, sources=sources)
```

And then start some jobs:

```r
tasks <- tasks_growth(iter = 10) # Set to 20 for testing, set to 1000 for actual deployment
create_dirs(unique(dirname(tasks$filename)))
enqueue_bulk(tasks, model_compiler, obj) # This will compile each model but won't go any further due to exhausting virtual memory issues.
```

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
