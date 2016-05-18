The instructions below provide details for rerunning the analyses we undertook. Running all these models takes weeks of compute time. If you simply wish to reproduce our analyses please following the instructions in the README within the parent directory.

# Rerunning mortality analysis
In this project we use 10-fold cross-validation on 15 different tropical tree Bayesian mortality models. Each model consisted of 3 MCMC chains, meaning that a we fitted 150 models (or 450 chains). To fit these models in a timely manner, we used the [Amazon Web Service Cluster](https://aws.amazon.com/about-aws/) coupled with [docker](https://www.docker.com). 
Here we outline the workflow we used to run these models on the AWS cluster. Following this we then run the final model using the same workflow but on our local computer, again using docker to minimise cross platform issues.

## Setting up local machine
Before running either workflow we first must install [docker](https://www.docker.com) and [remake](https://github.com/richfitz/remake).

### Installing remake
[remake](https://github.com/richfitz/remake) is a package that allows us to use a make-like workflow in R by specifying a series of declarative actions. In essence, this package tells R what order things should be run in to ensure all dependencies are met.  To install these packages enter R and run:

```
devtools::install_github("richfitz/remake")
install.packages(c("R6", "yaml", "digest", "crayon", "optparse"))
devtools::install_github("richfitz/storr")
devtools::install_github("richfitz/remake")
remake::install_missing_packages()
devtools::install_github("traitecoevo/dockertest")
```

NOTE: if `devtools` is not installed it can be by running `install.packages("devtools")`.

### Process data ready for analysis
Now we need to download, process and split the data into 10-folds for subsequent use with stan. We've automated this using the package `remake` and its declaration file `remake.yml`.  For a tutorial of how to use `remake` please see [here](https://github.com/ropenscilabs/remake-tutorial).

Ensuring you are within `mortality_bci` you can enter R and run the following:

```
R # enters R
remake::make() # runs remake
```

### Installing docker

Docker is a program that allows users to makes virtual machines (called containers) that contains all the software needed to run a program or analysis. As such it is a tool that can be used to guarantee some software will always run the same way, regardless of the environment it is running in. Instructions on how to install this can be found [here](https://docs.docker.com).

### Create a docker container

Once docker is installed we can build a docker container (a virtual machine). In our case, because we want to compile rstan models we require a container with sufficient amount of memory. 
Below we quit out of R and build a docker container with 6 GB of ram and 3 CPUs (the exact number will depend on how many cores you are happy to use) in the terminal using the Docker command `docker-machine`. We call this container  `mem6GB`

```
q("no") # quits R
docker-machine create --virtualbox-memory "6000" --driver virtualbox --virtualbox-cpu-count 3 mem6GB
```
(`--virtualbox-memory "6000"` sets how much virtual memory is available to the virtual machine.
 `----virtualbox-cpu-count 3` sets the number of CPUs you wish to use from your local machine)

### Precompile stan models for docker container

It is best to precompile our cross validated models for use with docker prior to actually sampling the models. This can be done in R (assuming you are within `mortality_bci`):

```
R
remake::make('crossval_models_precompiled_docker')
```

The following instructions are broken into two parts. For those who wish to run the entire cross valdiation proceedure read on. For those who wish to fit the final model please skip to the section 'fitting final model'.

# Cross validation proceedure (using AWS EC2 cluster)

### Installing Clusterous
Clusterous is a easy-to-use command line tool for cluster computing on AWS. It allows you to create and manage a cluster on AWS and deploy your software in the form of Docker containers. It is aimed at scientists and researchers who want the on-demand compute power that AWS offers, but don't have the necessary time or technical expertise.

A simple guide on how to install clusterous can be found [here](https://github.com/sirca/clusterous/blob/master/docs/manual/02_Quick_start.md)

### Start up AWS cluster

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
clusterous create docker/mycluster.yml
```

Once the create command finishes, run the status command to get an overview of your cluster:
```
clusterous status
```

The status command shows you, amongst other things, the number and types of nodes running on your cluster. Note the special Controller and NAT instances: these are part of each cluster and assist in the networking and management of the cluster, and you can safely ignore them for now.

You now have a working Clusterous cluster running on AWS.

Now we use the run command to launch the environment on the current running cluster:
```
clusterous run docker/clusterous_env.yml
```

When you run this, it will take a few minutes to copy some files over to your cluster, build a Docker image on the cluster, run clusterous_env parallel across the master and workers, and then create an SSH tunnel so that you can access the web-based notebook from your computer. Once the command finishes running, you should see output similar to this:

```
Checking for Docker images...
Copying files...
Starting 80 instances of engine
Starting 1 instance of redis
Launched 2 components: redis, engine

Message for user:
To access redis, use this URL: http://localhost:6379
```

Test that things are working by running
```
redis-cli -p 6379 PING
```

which should return `PONG` (and not "Connection refused").

Then within this terminal we load `rrqueue` and state what packages and source code the jobs will require. 

```
library(rrqueue)
packages <- c("rstan","dplyr")
sources <- c("R/model.R",
             "R/stan_functions.R",
             "R/utils.R")
```

Now we connect to `redis` (our master) on AWS.

```
con <- queue("rrq", redis_host="localhost", packages=packages, sources=sources,  redis_port = 6379)
```

Lastly, sequentially submit jobs located in `docker/jobs.R`. For example:

```
func_growth_tasks <- tasks_2_run(comparison = 'function_growth_comparison',iter = 4000, 
                     path="/home/data")
res <- enqueue_bulk(func_growth_tasks, model_compiler, obj, progress_bar = TRUE)
```

Once the tasks are running a progress bar will appear.

If you want to access the cluster from a different computer ensure the machine is configured to use the same AWS account and region.
Then you can use `workon` via the terminal to access the cluster from the second machine:

```
clusterous workon mortality_cluster
```

Once completed results can be copied back to your local computer from the terminal by ensuring you are in `mortality_bci` and running:

```
clusterous get results .
```

These results should be saved in `mortality_bci/results/`.

# Fitting final model

Assuming the instructions in `Setting up local machine` have been run and that you are currently in the parent directory `mortality_bci`, we can now run the most predictive model as determined by the cross validation proceedure above. Because we are only running a single model with three MCMC chains, we no longer require the AWS cluster. Instead we will run this model locally in a docker container (to reduce cross platform issues with parallelisation - and so it follows the same workflow proceedure as the cross validated models above).

**NOTE** This does stage does not require running the cross validated proceedure above.

First we need to precompile this final model and the final model with just species random effects. We do this by running:

```
R
remake::make('fulldata_models_precompiled_docker')
```

### Setting up our master
Now we need to set up a master container that will act as a database that receives results as they complete.
For this container we use a database software called `redis`. We can install this directly from dockerhub by quitting R and running the following in a terminal:

```
q("no") # quits R
eval "$(docker-machine env mem6GB)"
docker run --name mortality_bci_redis -d redis
```

**Note** if you have previously started redis, you'll get an error with the previous command that looks like:
```
Error response from daemon: Conflict. The name "stan_crossval_demo_redis" is already in use by container 0e246cf9734d. You have to delete (or rename) that container to be able to reuse that name.
```
and will need to do the following:
```
eval "$(docker-machine env mem6GB)"
docker stop mortality_bci_redis
docker rm mortality_bci_redis
docker run --name mortality_bci_redis -d redis
```

### Setting up controller
Next we set up a controller from which we can create and queue jobs from. We do using the a terminal window in the parent directory of the project and running:

```
eval "$(docker-machine env mem6GB)"
docker run --rm --link mortality_bci_redis:redis -v ${PWD}:/home/data -it traitecoevo/mortality_bci:latest R
```
This will load R in mem6GB and allow you to load `rrqueue`, state what R packages you require and what source code needed to run the jobs. For example, if you only wanted to run the final model you can run the following within the R terminal.

**NOTE** In our analysis we also run a second model to the entire data which only consists of random effects. We ran this model to examine the proportion of species variation explained by wood density in the final model. If you wish to run this model you can run below but change `tasks_2_run(comparison="final_model", iter = 4000)` to  `tasks_2_run(comparison="final_base_growth_hazard_re", iter = 4000)`

```
library(rrqueue)
packages <- c("rstan","dplyr")
sources <- c("R/model.R",
             "R/stan_functions.R")

#Connect the controller container to the redis container.
con <- queue("rrq", redis_host="redis", packages=packages, sources=sources)

# Dataframe of jobs
tasks <- tasks_2_run(comparison="final_model", iter = 4000)

# Submit jobs
res <- enqueue_bulk(tasks, model_compiler, con, progress_bar = TRUE)
```
When the jobs are submitted a progress bar will appear. At this stage there should be no progress as we haven't launched any workers to do the jobs.

### Launch workers to run the analysis

Lastly, we create workers that ask for, and then undertake, jobs from the controller. Because the controller is still running, you'll need to open **new** terminal tabs (as many as the number of CPU's you've allocated to your docker container `mem6GB`) in the parent directory.

Then you run the following for each terminal:
```
eval "$(docker-machine env mem6GB)"
docker run --rm --link mortality_bci_redis:redis -v ${PWD}:/home/data -t traitecoevo/mortality_bci:latest rrqueue_worker --redis-host redis rrq
```

This will launch workers that will begin to run through your jobs. The progress of these jobs can be seen from the controller terminal. Also as jobs complete they will automatically be exported to your parent directory under `results`.
