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

These packages depend on several non CRAN packages. These include [`redis`](http://redis.io),
[`hiredis`](https://github.com/redis/hiredis),
[`RedisAPI`](https://github.com/ropensci/RedisAPI),
[`RedisHeartBeat`](https://github.com/richfitz/RedisHeartbeat),
[`storr`](https://github.com/richfitz/storr),
[`ids`](https://github.com/richfitz/ids),
[`callr`](https://github.com/traitecoevo/callr/tree/master)

## Installing redis
Currently `redis` does not officially support Windows. However, Microsoft Open Tech group have developed and maintaineds a Windows port that can be downloaded [here](https://github.com/MSOpenTech/redis).
On a Mac The easiest way to install `redis` is to open up a terminal and run the following:

```
wget http://download.redis.io/redis-stable.tar.gz
tar xvzf redis-stable.tar.gz
cd redis-stable
make
sudo make install
```
*Note* If you haven't installed `wget` a binary can be downloaded [here](http://rudix.org/packages/wget.html).


## Installing hiredis

Once `redis` is installed start `redis-server` by typing in a terminal window:
```
redis-server
```
Now download the latest tar.gz release of [`hiredis`](https://github.com/redis/hiredis/releases).
Open a new terminal window and move to the path `hiredis` was downloaded. Now run the following (replacing: `hiredis-0.XX.Y.tar.gz` and later `cd hiredis-0.XX.Y`  with the actual filenames

```
tar -zxvf hiredis-0.XX.Y.tar.gz
cd hiredis-0.XX.Y 
make
sudo make install
```
*NOTE* After installing the above, and if you are using OSX and `bash`, you will need to add `export DYLD_LIBRARY_PATH=/usr/local/lib` anywhere in your `~/.profile`. Create this file if it does not exist.
If you are using `zshell` add the above to your `~/.zshrc`.

## Installing remaining non-CRAN packages
Now we can install the remaining packages by opening R and running:

```
install.packages(c("RcppRedis", "R6", "digest", "docopt"))
devtools::install_github(c("ropensci/RedisAPI", "richfitz/RedisHeartbeat", "richfitz/storr", "richfitz/ids", "richfitz/remake"))
devtools::install_github("traitecoevo/rrqueue","traitecoevo/callr")
devtools::install_github("traitecoevo/dockertest")
```

*NOTE* that on Linux, some dependencies of these packages will require headers for curl (e.g., `libcurl4-openssl-dev`) and hiredis (e.g., libhiredis-dev).


Building the docker image requires a lot of memory because it must compile and install `rstan`.  On Windows/OSX, the default virtual box is not big enough, and you will need to create a virtual machine with more memory.

As such, the container requires 6GB for compilation. Using the terminal and the Docker command `docker-machine` we create a new Docker container with 6GB of virtual memory called `mem6GB`

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

First move to the parent directory `mortality_bci`.
So we can access remake outside of R we first run:

```
sudo Rscript -e 'remake::install_remake("/usr/local/bin")'
```
Then running:
```
remake
```

will build all the data that is required.  This will take a long time, and cloning might be better:

```
git clone git@github.com:traitecoevo/mortality_data.git data
```

### Pre-compiled models
Because compiling the models uses alot of RAM we first precompile them for subsequent use by running:
```
remake models_precompiled_docker
```

This requires building the docker image, so might not be a good idea.  Alternatively, clone:

```
git clone git@github.com:traitecoevo/mortality_models.git models
```

time will tell if that is a sensible thing to do (note it will not work well if you've already run the native precompiled models, gah.)

## Running the mortality analysis in docker containers

Pull (or build) the most recent copy of the `traitecoevo/mortality_bci` container (see previous section).

In the parent parent directory `mortality_bci`. We're now going to setup three different Docker containers.
*NOTE* If you're using a new terminal, don't forget to set path variables by rerunning: `eval "$(docker-machine env mem6GB)"`


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

Then in R, add the jobs listed in `AWS_launch/start_jobs.R` and compile the stan models:

```r
library(rrqueue)
packages <- c("rstan")
sources <- c("R/model.R",
             "R/stan_functions.R",
             "R/utils.R")

# FIRST RUN GROWTH COMPARISON ANALYSIS
obj <- queue("rrq", redis_host="redis", packages=packages, sources=sources)
# On the cluster redis queue is called redis.marathon.mesos, but from local machine called localhost -- via exposed tunnel
growth_tasks <- tasks_2_run(analysis = 'growth_comparison',iter = 1000, 
                     growth_measure = c('true_dbh_dt','true_basal_area_dt'),
                     path="/home/data")
# queue the jobs
res <- enqueue_bulk(growth_tasks, model_compiler, obj, progress_bar = FALSE)

# NEXT FIND THE MOST PARIMONIOUS COMBINATION OF wood densities
# true_dbh_dt has a higher log likelihood and thus is used for subseqent analysis.
rho_tasks <- tasks_2_run(analysis = 'rho_combinations',iter = 1000, 
                     growth_measure = 'true_dbh_dt',
                     path="/home/data")
res <- enqueue_bulk(rho_tasks, model_compiler, obj, progress_bar = FALSE)

# NULL MODEL
# What about a model with just a constant hazard?
null_tasks <- tasks_2_run(analysis = 'null_model',iter = 1000, 
                     growth_measure = 'true_dbh_dt',
                     path="/home/data")
res <- enqueue_bulk(null_tasks, model_compiler, obj, progress_bar = FALSE)

# NULL MODEL VARIES BY SPECIES ID
null_re_tasks <- tasks_2_run(analysis = 'null_model_random_effects',iter = 1000, 
                     growth_measure = 'true_dbh_dt',
                     path="/home/data")
res <- enqueue_bulk(null_re_tasks, model_compiler, obj, progress_bar = FALSE)

# TRADITIONAL NEGATIVE EXPONENTIAL WITHOUT GAMMA

no_gamma_tasks <- tasks_2_run(analysis = 'no_gamma_model',iter = 1000, 
                     growth_measure = 'true_dbh_dt',
                     path="/home/data")
res <- enqueue_bulk(no_gamma_tasks, model_compiler, obj, progress_bar = FALSE)

# TRADITIONAL NEGATIVE EXPONENTIAL WITHOUT GAMMA BUT WITH RANDOM EFFECTS

no_gamma_re_tasks <- tasks_2_run(analysis = 'no_gamma_model_random_effects',iter = 1000, 
                     growth_measure = 'true_dbh_dt',
                     path="/home/data")
res <- enqueue_bulk(no_gamma_re_tasks, model_compiler, obj, progress_bar = FALSE)
```

Things to note here:

* `redis_host="redis"` points the queue at Redis running on the machine "redis", which is the name of the linked container
* The queue name is `rrq` but you can use whatever you fancy.

Which will display a progress bad with a spinner on the right hand side.

The precompilation is a bit tricky: it will persist to disk, but will only reliably work on when loaded from the docker container.


Third, we create workers that ask for, and then undertake, jobs from the controller.  Because the controller is still running (it actually does not ned to be), you'll need to open a new terminal window in the directory `mortality_bci/docker`.

Now workers can be launched via dockertest by running:

```
./dockertest launch --machine mem6GB --link mortality_bci_redis:redis -- rrqueue_worker --redis-host redis rrq
```

* The `--` separates options to dockertest from the program to run and its options.

The arguments here are identical to the controller above, except for the command to run within the container:

* `rrqueue_worker --redis-host redis rrq`

The `rrqueue_worker` command is built into `rrqueue` and creates a worker, looking for Redis on the host "redis" (which is the linked container) and reading jobs from the queue "rrq" (which is what the controller added jobs to above).

If you want to run multiple workers, open up more terminal tabs/windows and run the above code. (Currently we can only run 2 workers because the jobs require a lot of memory, due to compiling C++ code and rstan being a memory hungry monster).

Alternatively workers may be launched without dockertest by running the following within the parent directory `mortality_bci`:
```
eval "$(docker-machine env mem6GB)"
docker run --rm --link mortality_bci_redis:redis -v ${PWD}:/home/data -t traitecoevo/mortality_bci:latest rrqueue_worker --redis-host redis rrq
```

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

## Using with clusterous

Spin up the cluster with:

```
clusterous start mycluster.yml
clusterous launch clusterous_env.yaml
```

You should see:

```
Checking for Docker images...
Copying files...
Starting 2 instance of worker
Starting 1 instance of redis
Launched 2 components: redis, worker

Message for user:
To submit jobs to redis use this URL:http://localhost:31379
```

Test that things are working by running


```
redis-cli -p 31379 PING
```

which should return `PONG` (and not "Connection refused").

From within the mortality_bci directory, run `R`

```
library(rrqueue)
packages <- c("rstan")
sources <- c("R/model.R",
             "R/task_compiler.R",
             "R/stan_functions.R",
             "R/utils.R")
obj <- queue("rrq", redis_port="31379", packages=packages, sources=sources)
```

Ideally this will print something like:

```
creating new queue
X workers available

Attaching package: ‘inline’

The following object is masked from ‘package:Rcpp’:

    registerPlugin

rstan (Version 2.7.0-1, packaged: 2015-07-17 18:12:01 UTC, GitRev: 05c3d0058b6a)
For execution on a local, multicore CPU with excess RAM we recommend calling
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
```

`X workers available` is the number of workers that identified themselves; that should really match the number in "Starting X instance of worker" (this should be 2 with our current configuration).

Try queing a realaly stupid job:
```
t <- obj$enqueue(sin(1))
```

wait a second and check its status:

```
t$status()
```

Hopefully this will report `COMPLETE` and not an error.  If there are issues getting the local and remote copies of the application to agree on files then you'll see `ENVIR_ERROR` here, and the remote worker will actually exit.

```
t$result() # 0.841471
```

Then for something more challenging:

```
tasks <- tasks_growth(iter = 10)[1:10, ]
res <- enqueue_bulk(tasks, model_compiler, obj)
```

This will run with a spinner and a progress bar.  You don't have to leave it open, especially as we don't do anything useful with it.

```
obj$workers_log_tail(n=Inf)
```

Then we can run the whole lot:

```
tasks <- tasks_growth(iter = 1000)
res <- enqueue_bulk(tasks, model_compiler, obj)
```

You can hit `Ctrl-C` here once the progress bar appears and stop the progress bar

Once the tasks are running you can run:

```
obj$tasks_overview()
```

to see how many tasks have run / will run, and

```
obj$tasks_times()
```

to get information on waiting, running, times (`options(width=120)` might be needed if you resize your terminal window; will be automatic in Rstudio).
