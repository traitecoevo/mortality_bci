# Running the analysis in docker

Pull the most recent copy of the `traitecoevo/mortality_bci` image:

```
docker pull traitecoevo/mortality_bci
```

Start a container running Redis:

```
docker run --name mortality_bci_redis -d redis
```

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
tasks <- tasks_growth(iter = 100)
create_dirs(unique(dirname(tasks$filename)))
enqueue_bulk(tasks, model_compiler, obj)
```
