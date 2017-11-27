# Unifying intra- and inter-specific variation in tropical tree mortality
*By:* James S Camac, Richard Condit, Richard G FitzJohn, Lachlan McCalman,
Daniel Steinberg, Mark Westoby, Joe Wright, Daniel Falster

*Maintainers:* James Camac and Daniel Falster

## Background

Tree death is a fundamental process driving population dynamics, nutrient cycling, and evolution within plant communities. While past research has identified factors influencing tree mortality across a variety of scales, these distinct drivers are yet to be integrated within a unified predictive framework. In this study, we use a cross-validated Bayesian framework coupled with classic survival analysis techniques to derive instantaneous mortality functions for 203 tropical rainforest tree species at Barro Colorado Island (BCI) Panama. Specifically, we develop mortality functions that not only integrate individual, species, and temporal effects, but also partition the contributions of growth-dependent and growth-independent effects on the overall instantaneous mortality rate.

This repository contains the data and code required to reproduce our entire workflow from data cleaning, rerunning the analysis, producing figures and reproducing the manuscript. Details below instructions on how this work can be reproduced.


## Dataset
This paper uses data from  Barro Colorado Island (BCI) Panama. 
Condit, R., Lao, S., Pérez, R., Dolins, S.B., Foster, R.B. Hubbell, S.P. 2012. Barro Colorado Forest Census Plot Data, 2012 Version. DOI http://dx.doi.org/10.5479/data.bci.20130603

## Preprint
A preprint of this project has been released on BioRxiv: 

## Reproducing analysis
We are committed to reproducible science. As such, this repository contains all the data and code necessary to fully reproduce our results. To facilitate the reproducibility of this work, we have created a docker image and set up the entire workflow using [remake](https://github.com/richfitz/remake). Below we outline the two approaches that can be taken to reproduce the analyses, figures and manuscript.

## Reproducing analysis without docker

### Installing remake

First install some dependencies from cran as follows:

```r
install.packages(c("R6", "yaml", "digest", "crayon", "optparse"))
```

Now we'll install some packages from [github](github.com). For this, you'll need the package [devtools](https://github.com/hadley/devtools). If you don't have devtools installed you will see an error "there is no package called 'devtools'"; if that happens install devtools with `install.packages("devtools")`.


Then install the following two packages

```r
devtools::install_github("richfitz/storr")
devtools::install_github("richfitz/remake")
```
See the info in the [remake readme](https://github.com/richfitz/remake) for further details if needed.

### Copy repository
First copy the repository to your a desired directory on you local computer. 

This can either be done using the terminal (assuming git is installed)

```
git clone https://github.com/traitecoevo/mortality_bci.git
```

Or can be downloaded manually by clicking [here](https://github.com/traitecoevo/mortality_bci/archive/master.zip).

Now open a new R session with this project set as working directory. We use a number of packages, these can be easily installed by remake:

```r
remake::install_missing_packages()
```

Then run the following to generate all outputs (figures, table, knitr report):

```r
remake::make()
```

For comparison, also included is a traditional script `Rscript.R` that generates the same outputs, without using remake. This script is automatically generated from the `remake.yml` file, as part of the remake workflow.


## Reproducing analysis with remake & docker

Each computer is different. Operating systems, software install and the versions of such software are likely to vary substantially between computers. As such it is extremely difficult to develop code that can easily run on all computers. This is where Docker comes in. [Docker](https://www.docker.com/what-docker) is the world’s leading software container platform.  Here we use Docker because it can readily be used across platforms and is set to install the appropriate software, and software versions used in the original analysis. As such it safeguards code from differences among computers and potential changes in software and cross platform issues.

### Setting up Docker
If you haven't installed docker please see [here](https://www.docker.com/products/overview).

We can set up docker two ways. If a docker image already exists on Docker Hub you can just download it by running something like the following:

```
docker pull jscamac/mortality_bci
```
This image contains all required software to run this analysis.


If however, the image is not available or you've created your own Dockerfile you can build it. This will be slower relative to the `docker pull` approach as it requires compiling the whole image.

```
docker build --rm --no-cache -t mortality_bci .

```
**The period is important as it tells docker to look for the dockerfile in the current directory**

## Using docker
Now we are all set to reproduce this project!

Now there are two common ways we can interface with the docker container:
If your image only includes base `R` you can access it via the terminal by opening a terminal and running:

### Accessing the docker terminal
If you're comfortable with running work directly via R from a terminal or your image only includes base `R` run:

```
docker run -v /Users/path/to/repository/:/home/mortality_bci  -it jscamac/mortality_bci

```

### Accessing Rstudio from within docker
If you are more comfortable with running code using a GUI interface such as Rstudio, you can open set run docker such that you interface directly with Docker's Rstudio.
To do this you must have a docker image that contains Rstudio.

```
docker run -v /Users/path/to/repository/:/home/rstan_build:/home/rstudio -p 8787:8787 mortality_bci
```
Now just open your web browser and go to the following: `localhost:8787/`

The username and password is `rstudio`

### Rerunning analysis from within docker
Now the final stage is to rerun the entire workflow by simply running:

```
remake::make()
```


## Problems?
If you have any problems getting the workflow to run please create an issue and I will endevour to remedy it ASAP.