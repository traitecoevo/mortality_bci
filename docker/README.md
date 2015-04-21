# Docker

Load a couple of helper functions

```sh
. ./helpers.sh
```

To build the image:

```sh
Rscript -e 'dockertest::build()'
```

or pull the image image from docker hub:

```sh
docker pull richfitz/mortality_bci:latest
```

Building the image will clone the source into `self`, but if it's out of date, this will refresh it

```sh
clone_or_pull ../ self
```

Starting both the Redis container and worker container:

```sh
docker-compose up
```

or for multiple workers

```sh
docker-compose scale redis=1 worker=4
```

In another terminal, launch the controller (this part is the biggest pain)
```
. ./helpers.sh
boot2docker_init
docker run --link docker_redis_1:redis -v ${PWD}/self:/root/mortality_bci -it richfitz/mortality_bci:latest R
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

## Building notes

Building the docker image takes a lot of memory, thanks to `rstan`; allow at least 3GB for compilation to succeed.  Under `boot2docker` that can be done by editing the machine setttings in the VirtualBox manager (Cmd-S and adjust the slider under System) or by running

```
VBoxManage modifyvm boot2docker-vm --memory 3000
```

(you'll need to restart `boot2docker` for the changes to take effect.)

We autogenerate the Dockerfile using [dockertest](https://github.com/richfitz/dockertest); the main configuration file is `docker/dockertest.yml`, which is declarative rather than a list of instructions.

To build the image, run

```
dockertest::build()
```

which can take a while.  This builds the image `richfitz/mortality_bci` which we also push to dockerhub so that

```
docker pull richfitz/mortality_bci:latest
```

should also work.

Both the worker and the controller use this image, but the workers run `rrqueue_worker_tee` rather than an interactive R session.

## Building steps

```R
devtools::load_all("~/Documents/src/dockertest")
dockertest::prepare(dockertest::project_info("test"))
dockertest::build()
```
