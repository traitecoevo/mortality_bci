# Using docker

Goal here is run our mortality analysis in docker containers. Docker containers wrap up a piece of software in a complete filesystem that contains everything it needs to run: code, runtime, system tools, system libraries – anything you can install on a server. This guarantees that it will always run the same, regardless of the environment it is running in.

## Installing and setting up docker

First install docker. Docker can be installed on OSX, Windows and Linux. A comprehensive guide can be found [here](http://docs.docker.com/mac/started/).

Once installed, Docker can be opened either via `Docker Quickstart Terminal` found in the docker folder in the applications folder, or accessed directly via the terminal using `docker-machine` followed by Docker commands. The instructions below use `docker-machine` and the terminal.

**Note** Multiple docker containers can be made. Details on how to do this are available [here](https://docs.docker.com/installation/mac/).

## Building/retrieving the `traitecoevo/mortality_bci` Docker container

Not all of this is strictly necessary, and it depends how you want to interact with the system.

* To build the docker image you will need the [`dockertest`](https://github.com/traitecoevo/dockertest) package
* To connect to the image or interact with the queuing system from the host you will need the [`rrqueue`](https://github.com/traitecoevo/rrqueue) packages.

These have a set of dependencies that are not on CRAN, and the simplest way to install is via our [`drat`](https://github.com/traitecoevo/drat) repository (`drat` itself can be installed with `install.packages("drat")`)

```
drat:::add("traitecoevo")
install.packages(c("rrqueue", "dockertest"))
```

Note that on Linux, some dependencies of these packages will require headers for curl (e.g., `libcurl4-openssl-dev`) and hiredis (e.g., libhiredis-dev).

Alternatively, to install directly from github:

```
devtools::install_github(c("traitecoevo/dockertest", "ropensci/RedisAPI", "richfitz/storr", "traitecoevo/rrqueue"))
```

Building the docker image requires a lot of memory because it must compile and install `rstan`.  On Windows/OSX, the default virtual box is not big enough, and you will need to create a virtual machine with more memory

As such, the container require 6GB for compilation. Using the terminal and the Docker command `docker-machine` we create a new Docker container with 6GB of virtual memory called `mem6GB`

```
docker-machine create --virtualbox-memory "6000" --driver virtualbox mem6GB
```

(`--virtualbox-memory "6000"` sets how much virtual memory is available to the virtual machine.)

We auto generate the Dockerfile using [dockertest](https://github.com/traitecoevo/dockertest); the main configuration file is `docker/dockertest.yml`, which is declarative rather than a list of instructions.

To build the `traitecoevo/mortality_bci` docker container from the terminal first move into the folder `mortality_bci/docker` and then run:

```
./dockertest build --machine mem6GB
```

which will arrange connecting to your new, larger VM and build the image.

The build can take a while, so we have pushed a pre-built image to dockerhub, which you can retrieve via the terminal using:

```
docker pull traitecoevo/mortality_bci:latest
```

(**Note** To push a docker container run `docker login`, then ` docker push traitecoevo/mortality_bci`.)

Below we setup 'workers' and a 'controller' using this image, but the workers run `rrqueue_worker_tee` rather than an interactive R session.

Building the image will also clone the source into the folder `self`, but if it's out of date, it will refresh it

## Building the prerequsites

### Pre-processed data

Runing

```
remake::make()
```

will build all the data that is required.  This will take a long time, and cloning might be better:

```
git clone git@github.com:traitecoevo/mortality_data.git data
```

### Pre-compiled models

```
remake::make("models_precompiled_docker")
```

This requires building the docker image, so might not be a good idea.  Alternatively, clone:

```
git clone git@github.com:traitecoevo/mortality_models.git models
```

time will tell if that is a sensible thing to do (note it will not work well if you've already run the native precompiled models, gah.)

## Running the mortality analysis in docker containers

Pull (or build) the most recent copy of the `traitecoevo/mortality_bci` container (see previous section).

Then move your working directory from `mortality_bci/docker` back to the parent directory `mortality_bci`. And if you're starting in a new terminal, don't forget to set path variables:

```
eval "$(docker-machine env mem6GB)"
```

We're now going to setup three different Docker containers.

First, we start a container running Redis - this will sit in the background and act as a database catching results:

```
docker run --name mortality_bci_redis -d redis
```

The `-d` flag runs Redis in *daemon* mode (i.e., in the background).

We name the container (`--name mortality_bci_redis`) so that we can *link* it to other containers.

**Note** if you have previously started redis, you'll get an error with the previous command that looks like:

```
Error response from daemon: Conflict. The name "mortality_bci_redis" is already in use by container 0e246cf9734d. You have to delete (or rename) that container to be able to reuse that name.
```

and will need to do the following:
```

docker stop mortality_bci_redis
docker rm mortality_bci_redis
docker run --name mortality_bci_redis -d redis
```

Second, we start a container called 'controller' from which we can create and queue jobs _from_. First launch the container and start R:

```
docker run --rm --link mortality_bci_redis:redis -v ${PWD}:/home/data -it traitecoevo/mortality_bci:latest R
```

The components of this command are:

* `--rm` - remove the container once we're done
* `--link mortality_bci_redis:redis` - the `mortality_bci_redis` container (started above) will be available in this container with the name `redis`; i.e., Redis will appear to be running on `redis:6379`
* `-v ${PWD}:/home/data` - this maps the current working directory on the host machine to `/home/data` in the container, which is the working directory for this container
* `-it` launches in interactive mode, which allows Ctrl-C to enable killing the worker process
* `traitecoevo/mortality_bci:latest` is the image created above
* `R` run R rather than `bash`, which is the default for this image.

Then in R, add the jobs and compile the stan models:

```r
library(rrqueue)
packages <- c("rstan")
sources <- c("R/model.R",
             "R/task_compiler.R",
             "R/stan_functions.R",
             "R/utils.R")
obj <- queue("rrq", redis_host="redis", packages=packages, sources=sources)

tasks <- tasks_growth(iter = 10) # Set to 10 for testing, set to 1000 for actual deployment
res <- enqueue_bulk(tasks, model_compiler, obj)
```

Things to note here:

* `redis_host="redis"` points the queue at Redis running on the machine "redis", which is the name of the linked container
* The queue name is `rrq` but tou can use whatever you fancy.

Which will display a progress bad with a spinner on the right hand side.

The precompilation is a bit tricky: it will persist to disk, but will only reliably work on when loaded from the docker container.

Third, we create workers that ask for, and then undertake, jobs from the controller.  Because the controller is still running (it actually does not ned to be), you'll need to open a new terminal window, also in the project root.

```
eval "$(docker-machine env mem6GB)"
docker run --rm --link mortality_bci_redis:redis -v ${PWD}:/home/data -t traitecoevo/mortality_bci:latest rrqueue_worker --redis-host redis rrq
```

Alternatively, this can be launched via dockertest:

```
dockertest launch --machine mem6GB --link mortality_bci_redis:redis -- rrqueue_worker --redis-host redis rrq
```

* The `--` separates options to dockertest from the program to run and its options.

The arguments here are identical to the controller above, except for the command to run within the container:

* `rrqueue_worker --redis-host redis rrq`

The `rrqueue_worker` command is built into `rrqueue` and creates a worker, looking for Redis on the host "redis" (which is the linked container) and reading jobs from the queue "rrq" (which is what the controller added jobs to above).

If you want to run multiple workers, open up more terminal tabs/windows and run the above code. (Currently we can only run 2 workers because the jobs require a lot of memory, due to compiling C++ code and rstan being a memory hungry monster).

## Preparing for use with clusterous

Very closely related to the above, but designed to test use with clusterous.

Within the `docker/` directory, in the shell run the command

```
docker_clusterous.R
```

which will build a little container `traitecoevo/mortality_bci_clusterous`; this will include _only_ the files that will be copied to the host.  It might be tidier to do this with a data-only container, so this might change soon (a data-only container might simplify things).

```
docker run --name mortality_bci_redis -d redis
docker run --rm --link mortality_bci_redis:redis -t traitecoevo/mortality_bci_clusterous
```

and

```
docker run --rm --link mortality_bci_redis:redis -v ${PWD}:/home/data -it traitecoevo/mortality_bci:latest R
```

then

```
library(rrqueue)
packages <- c("rstan")
sources <- c("R/model.R",
             "R/task_compiler.R",
             "R/stan_functions.R",
             "R/utils.R")
obj <- queue("rrq", redis_host="redis", packages=packages, sources=sources)
tasks <- tasks_growth(iter = 10)
res <- enqueue_bulk(tasks, model_compiler, obj)
```
