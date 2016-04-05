# Rerunning mortality analysis
In this project we use 10 k-fold cross-validation on 15 different models, with each model sampled using three MCMC chains. In total this meant that we fitted 150 models (or 450 chains). To fit these models in a timely manner, we used the [Amazon Web Service Cluster](https://aws.amazon.com/about-aws/) coupled with [docker](https://www.docker.com). 
Below we outline:
1) how to set up your machine to submit jobs to the docker container;
2) how to build the docker container used for running the models; and
3) how to run models within the docker container on either your local machine (Not recommended) or via the AWS cluster.


## Setting up local machine

To rerun these mortality models using docker either locally or on the AWS cluster we first require the installation of:
[docker](https://www.docker.com);
[redis](http://redis.io);
[hiredis](https://github.com/redis/hiredis); and
[remake](https://github.com/richfitz/remake);

### Installing docker

Docker is a program that allows users to makes virtual machines (called containers) that contains all the software needed to run a program or analysis. As such it is a tool that can be used to guarantee some software will always run the same way, regardless of the environment it is running in.

It can be installed by running the following in the terminal:

```
wget -qO- https://get.docker.com/ | sh
```

### Installing redis
`redis` is a database. Currently it does not officially support Windows. However, Microsoft Open Tech group have developed and maintain a Windows port that can be downloaded [here](https://github.com/MSOpenTech/redis).
On a Mac The easiest way to install `redis` is to open up a terminal and run the following:

```
wget http://download.redis.io/redis-stable.tar.gz
tar xvzf redis-stable.tar.gz
cd redis-stable
make
sudo make install
```
*Note* If you haven't installed `wget` a binary can be downloaded [here](http://rudix.org/packages/wget.html).

Once `redis` is installed start `redis-server` by typing in a terminal window:
```
redis-server
```

### Installing hiredis

Download the latest tar.gz release of [`hiredis`](https://github.com/redis/hiredis/releases).
Open a new terminal window and move to the path `hiredis` was downloaded. Now run the following (replacing: `hiredis-0.XX.Y.tar.gz` and later `cd hiredis-0.XX.Y`  with the actual filenames

```
tar -zxvf hiredis-0.XX.Y.tar.gz
cd hiredis-0.XX.Y 
make
sudo make install
```
*NOTE* After installing the above, and if you are using OSX and `bash`, you will need to add `export DYLD_LIBRARY_PATH=/usr/local/lib` anywhere in your `~/.profile`. Create this file if it does not exist.
If you are using `zshell` add the above to your `~/.zshrc`.


### Installing remake
remake is a package that allows us to use a make-like workflow in R by specifying a series of declarative actions. In essence, this package tells R what order things should be run in to ensure all dependencies are met. This package can be installed by running the following in R:

```
devtools::install_github("richfitz/remake", dependencies=TRUE)
```

We also need to install other important R packages to aid in running the analysis on the cluster or locally.

```
remake::install_missing_packages()
install.packages(c("RcppRedis","docopt"))
devtools::install_github(c("ropensci/RedisAPI", "richfitz/RedisHeartbeat", "richfitz/ids"))
devtools::install_github("traitecoevo/rrqueue")
devtools::install_github("traitecoevo/dockertest")
```

### Installing Clusterous (only needed if using AWS)
Clusterous is a easy-to-use command line tool for cluster computing on AWS. It allows you to create and manage a cluster on AWS and deploy your software in the form of Docker containers. It is aimed at scientists and researchers who want the on-demand compute power that AWS offers, but don't have the necessary time or technical expertise.

A simple guide on how to install clusterous can be found [here](https://github.com/sirca/clusterous/blob/master/docs/manual/02_Quick_start.md)

## Preparing docker container

Our project requires alot a container that contains at least 6GB of memory to compile and install all packages and software. Because the default container that is created upon the installation of docker is not big enough to meet our needs we create a larger docker container called `mem6GB`

Using the terminal and the Docker command `docker-machine` we create a new Docker container with 6GB of virtual memory called `mem6GB`

```
docker-machine create --virtualbox-memory "6000" --driver virtualbox mem6GB
```

Once we have an adequate sized docker container we next add the software to it by either creating a docker image (a snapshot of what software is needed).
To rebuild the docker image move into `mortality_bci/docker/` and run from the terminal:

```
Rscript -e "library(methods); dockertest:::main(list('build', '--machine mem6GB'));"
```

The above will connect to the `mem6GB` container and use dockertest to build the docker image which it will then save in `docker/Dockerfile`.
(**Note**: if the installed R packages have changed substantially, this won't be detected by dockertest, so you'll want to rebuild with `--no-cache` flag added).

Now that we have a docker container with an image of the mortality working directory we now process the data and then precompile the stan models so that workers don't need to recompile a model each and everytime it gets a new job.
This can be done by first moving back to the parent directory `mortality_bci` and running the following in R:

```
remake::make()
remake::make('models_precompiled_docker')
```

Assuming your terminal is still in the parent directory `mortality_bci`. We now setup three different Docker containers: 
1) A container with Redis that acts as a database catching results as they complete 
2) A controller from which we can create and queue jobs from.
3) Worker containers that run the jobs queued by the controller


First, we start a container called `mortality_bci_redis`:

```
eval "$(docker-machine env mem6GB)"
docker run --name mortality_bci_redis -d redis
```

**Note** if you have previously started redis, you'll get an error with the previous command that looks like:

```
Error response from daemon: Conflict. The name "mortality_bci_redis" is already in use by container 0e246cf9734d. You have to delete (or rename) that container to be able to reuse that name.
```

and will need to do the following:

```
eval "$(docker-machine env mem6GB)"
docker stop mortality_bci_redis
docker rm mortality_bci_redis
docker run --name mortality_bci_redis -d redis
```

Next, we start a container called 'controller' and start R:

```
eval "$(docker-machine env mem6GB)"
docker run --rm --link mortality_bci_redis:redis -v ${PWD}:/home/data -it traitecoevo/mortality_bci:latest R
```

Then within this terminal we load `rrqueue` and state what packages and source code the jobs will require. 

```
library(rrqueue)
packages <- c("rstan","dplyr")
sources <- c("R/model.R",
             "R/stan_functions.R",
             "R/utils.R")
```

From here we can either run the workers on a local machine or on a cluster


###Run models locally using docker

Connect the controller container to the redis container.

```
obj <- queue("rrq", redis_host="redis", packages=packages, sources=sources)
```

Now submit a list of jobs to be completed. For example below we submit the task function_growth_comparison which will run 3 model forms with k-fold cross validation for two growth measures.
**NOTE** We run three levels of model comparison sequentially. To run other comparisons use code located in `AWS_launch/start_jobs`.

```
func_growth_tasks <- tasks_2_run(comparison = 'function_growth_comparison',iter = 4000, 
                     path="/home/data")
res <- enqueue_bulk(func_growth_tasks, model_compiler, obj, progress_bar = TRUE)
```

Lastly, we create workers that ask for, and then undertake, jobs from the controller.  Because the controller is still running (it actually does not ned to be), you'll need to open a new terminal tab in the directory `mortality_bci`.

Now workers can be launched via dockertest by running:

```
eval "$(docker-machine env mem6GB)"
./docker/dockertest launch --link mortality_bci_redis:redis -- rrqueue_worker --redis-host redis rrq
```

Alternatively workers may be launched without dockertest by running the following within the parent directory `mortality_bci`:
```
eval "$(docker-machine env mem6GB)"
docker run --rm --link mortality_bci_redis:redis -v ${PWD}:/home/data -t traitecoevo/mortality_bci:latest rrqueue_worker --redis-host redis rrq
```

## Run models on AWS using clusterous

Clusterous creates clusters on your own AWS account, so before you start using Clusterous, you need provide your AWS credentials and run the interactive Clusterous setup wizard.

The AWS keys are in the form of an Access Key ID and Secret Access Key, and are provided to you by AWS when you create an IAM user in your AWS account. You may refer to this [manual's guide](https://github.com/sirca/clusterous/blob/master/docs/manual/A02_AWS.md) to preparing your AWS account for use. Your AWS credentials will typically look something like:

```
Access Key ID: AKIAIOSFODNN7EXAMPLE
Secret Access Key: wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
```

Once you have obtained your AWS keys, run the setup command to launch the interactive wizard from the terminal:
```
clusterous setup
```
The setup wizard will start by asking you to enter your AWS keys (which you can copy/paste into the terminal). Following that, it will guide your through setting up some AWS resources that Clusterous needs for launching and managing clusters. In each case, Clusterous will give you the option of either picking an existing one or creating a new one. In general, if you expect to share your clusters with collaborators, it is recommended that you use the same resources as them. If you are the first to use Clusterous on your account, or will be working alone, feel free to create your own resources using the setup wizard.

Some of the questions the setup wizard will ask for:

- The AWS region you want to use. This corresponds to the location of their data centers, and you would typicaly choose the one geographically nearest to you.
- The VPC (Virtual Private Cloud) to use, which pertains to AWS networking. If you are the first to use AWS on your account, feel free to create a new one. If a colleague is already using Clusterous on your account, it is best to choose the same one as them
- A Key Pair, which is an SSH key for establishing a secure connection to your cluster. Clusterous needs a .pem key file to create and manage clusters. Like with VPCs, it is best to use an existing Key Pair if you are collaborating with others on your account. If chosing an existing Key Pair, Clusterous will ask you for the location of your key file.
- An S3 bucket for storing Docker images. Like with other resources, you are best off sharing the S3 bucket with collaborators. Note that S3 bucket names are global across AWS, so people typically prefix their organisation name.
- A name for this configuration profile. You need only one profile to begin with. Clusterous lets you have multiple sets of configuration, which is useful if you run on multiple regions or AWS accounts.
Once you have succesfully run the setup wizard, you are ready to start using Clusterous.


Create the cluster by entering
```
clusterous create mycluster.yml
```

Once the create command finishes, run the status command to get an overview of your cluster:
```
clusterous status
```

The status command shows you, amongst other things, the number and types of nodes running on your cluster. Note the special Controller and NAT instances: these are part of each cluster and assist in the networking and management of the cluster, and you can safely ignore them for now.

You now have a working Clusterous cluster running on AWS.

Now we use the run command to launch the environment on the current running cluster:
```
clusterous run clusterous_env.yaml
```

When you run this, it will take a few minutes to copy some files over to your cluster, build a Docker image on the cluster, run clusterous_env parallel across the master and workers, and then create an SSH tunnel so that you can access the web-based notebook from your computer. Once the command finishes running, you should see output similar to this:

```
Message for user:
The IPython engines may take up to 30 seconds to become available.
The connection file is located at:
/home/data/ipython/profile/security/ipcontroller-client.json
To access IPython notebook, use this URL: http://localhost:8888
```


Message for user:
To submit jobs to redis use this URL:http://localhost:31379
```

Test that things are working by running


```
redis-cli -p 31379 PING
```

which should return `PONG` (and not "Connection refused").


We are almost there! Now we need to start the controller and start R by running:

```
eval "$(docker-machine env mem6GB)"
docker run --rm --link mortality_bci_redis:redis -v ${PWD}:/home/data -it traitecoevo/mortality_bci:latest R
```

Then, like we did locally, we load `rrqueue` and state what packages and source code the jobs will require. 

```
library(rrqueue)
packages <- c("rstan")
sources <- c("R/model.R",
             "R/stan_functions.R",
             "R/utils.R")
```

and connect the controller to the redis.

```
obj <- queue("rrq", redis_host="localhost", packages=packages, sources=sources,  redis_port = 31379)
```
**NOTE** There is a difference is connecting the controller to redis when using redis. Specifically, we need to define the redis_port as 31379 and redis_host as `localhost`.

Now, within the terminal R session, the jobs can be submitted as:


```
func_growth_tasks <- tasks_2_run(comparison = 'function_growth_comparison',iter = 4000, 
                     path="/home/data")
res <- enqueue_bulk(func_growth_tasks, model_compiler, obj, progress_bar = TRUE)
```

Once the tasks are running you can run:

```
obj$tasks_overview()
```

to see how many tasks have run / will run, and

```
obj$tasks_times()
```

to get information on waiting, running, times (`options(width=120)` might be needed if you resize your terminal window; will be automatic in Rstudio).
