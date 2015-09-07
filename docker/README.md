# Using docker

Goal here is run analysis in docker containers. Docker is ....

## Installing and setting up docker

You'll need to have docker installed. On OSX we use docker-machine, which can be installed ..... Docker-machine also available for Windows on Linux (???).



## Building / retrieving the docker containers

Building the docker image takes a lot of memory, thanks to `rstan`; allow at least 3GB for compilation to succeed.  Under `docker-machine` that can be done by creating a new `mem3GB` machine

```
docker-machine create --softlayer-memory "3000" --driver virtualbox mem3GB
```

To use this machine then run

```
eval "$(docker-machine env mem3GB)"
```

We auto generate the Dockerfile using [dockertest](https://github.com/traitecoevo/dockertest); the main configuration file is `docker/dockertest.yml`, which is declarative rather than a list of instructions. The file `rstan.yml` contains some additional instructions specifying a new rstan module for dockertest.

To build the image, open up a terminal window in the `docker` folder, make sure docker is running, and then run

```
Rscript -e 'dockertest::build()'
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

Then start a container running Redis:

```
docker run --name mortality_bci_redis -d redis
```
(if you have previously started need to either delete container first using
`docker rm mortality_bci_redis`)

Start some worker containers listening on queue `rrq` (for now not running in daemon mode, though not actually interactive either; we'll do this with compose or something else later)

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
enqueue_bulk(tasks, model_compiler, obj)
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
