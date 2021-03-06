# Partitioning mortality into growth-dependent and growth-independent hazards across 203 tropical tree species
*By:* James S Camac, Richard Condit, Richard G FitzJohn, Lachlan McCalman,
Daniel Steinberg, Mark Westoby, Joe Wright, Daniel Falster

*Maintainers:* James Camac and Daniel Falster

## Background

We present a model that partitions rates of tropical tree mortality into growth-dependent and growth-independent hazards. This creates the opportunity to examine the relative contributions of within-species and across-species variation on tropical tree mortality rates, but also, how species traits affect each hazard. We parameterize this model using >400,000 observed survival records collected over a 15-year period at Barro Colorado Island from more than 180,000 individuals across 203 species. We show that marginal carbon budgets are a major contributor to tree death on Barro Colorado Island. Moreover, we found that while species' light demand, maximum dbh and wood density affected tree mortality in different ways, they explained only a small fraction of the total variability observed among species.

This repository contains the data and code required to reproduce our entire workflow from data cleaning, rerunning the analysis, producing figures and reproducing the manuscript. Details below instructions on how this work can be reproduced.


## Dataset
This paper uses three datasets collected from  Barro Colorado Island (BCI) Panama. 

Condit, R., Lao, S., Pérez, R., Dolins, S.B., Foster, R.B. Hubbell, S.P. 2012. Barro Colorado Forest Census Plot Data, 2012 Version. DOI http://dx.doi.org/10.5479/data.bci.20130603

Hubbell, S.P, Comita, L., Lao, S. Condit, R. 2014. Barro Colorado Fifty Hectare Plot Census of Canopy Density, 1983-2012. DOI http://dx.doi.org/10.5479/data.bci20140711.

BCI Wood density data measured and collated by Joe Wright (included within this repository).

## Publication
Camac, J.S., Condit, R., FitzJohn, R.G., McCalman, L., Steinberg, D., Westoby, M., Wright, S.J., Falster, D. (2018) Partitioning mortality into growth-dependent and growth-independent hazards across 203 tropical tree species. 115 (49) 12459-12464. DOI https://doi.org/10.1073/pnas.1721040115

## Preprint
A preprint prior to submission to PNAS was released on [BioRxiv](https://doi.org/10.1101/228361). This preprint uses a subset of the models that were eventually published in PNAS. These can be found in the first release of this project.

## Reproducing analysis
We are committed to reproducible science. As such, this repository contains all the data and code necessary to fully reproduce our results. To facilitate the reproducibility of this work, we have created a docker image and set up the entire workflow using [remake](https://github.com/richfitz/remake). Below we outline the two approaches that can be taken to reproduce the analyses, figures and manuscript.

### Copy repository
Running all model cross validations took approximately 2-months of computing time on a HPC machines. As the model fitting proceedure is not included in the remake workflow. We have however provided the model fits in a [FigShare repository](https://doi.org/10.6084/m9.figshare.7295486.v1). Please download this repository onto your computer. 

**Note:** You may also clone the repository directly from github. However, doing so requires manually downloading the results from the [latest release](https://github.com/traitecoevo/mortality_bci/releases/tag/v1.0) and placing them into the repository following the release notes. This extra step is due to file size constraints currently implemented on GitHub. 

## Reproducing analysis with remake & docker (Recommended approach)

Each computer is different. Operating systems, software install and the versions of such software are likely to vary substantially between computers. As such it is extremely difficult to develop code that can easily run on all computers. This is where Docker comes in. [Docker](https://www.docker.com/what-docker) is the world’s leading software container platform.  Here we use Docker because it can readily be used across platforms and is set to install the appropriate software, and software versions used in the original analysis. As such it safeguards code from differences among computers and potential changes in software and cross platform issues.

### Setting up Docker
If you haven't installed docker please see [here](https://www.docker.com/products/overview).

Because some of the data compilation is memory intensive. We suggest you allow docker to access at least 4GB of your systems RAM. This can easily be done within docker's settings and it's associated sliders.

We can set up docker two ways. The recommended approach is to download the precompiled docker image by running the following in the terminal/shell:

```
docker pull traitecoevo/mortality_bci
```
This image contains all required software (and software versions) to run this analysis.


If however, you would like the recompile the image from scratch the code below can be run. Note this will much slower relative to the `docker pull` approach.

```
docker build --rm --no-cache -t traitecoevo/mortality_bci .

```
**The period is important as it tells docker to look for the dockerfile in the current directory**

## Using docker
Now we are all set to reproduce this project!

### Rstudio from within docker
To be able to run the code, we interface with the Rstudio within the docker container by running the following from within the repository directory via the terminal/shell:

```
docker run -d -v $(pwd):/home/rstudio/ -p 127.0.0.1:8787:8787 \
-e DISABLE_AUTH=true traitecoevo/mortality_bci

```

**NOTE:**
Windows users may need to replace `$(pwd)` with the path to the downloaded repository or possibly `%cd%`.


Now just open your web browser and go to the following: `localhost:8787/`

### Rerunning analysis from within docker
One can now reproduce the outputs by running:

```
remake::make()
```

## Reproducing analysis without docker (Not recommended)
This option is not recommended as R packages are constantly being updated and backwards compatibility broken. However, if you are adverse to using Docker you can run `remake` outside docker and willing by using the instructions below. Code was developed under R 3.5.1 (2018-09-01).

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

Open a new R session with this project set as working directory. We use a number of packages, these can be easily installed by remake:

```r
remake::install_missing_packages()
```
However, the above may not work if packages have been updated and backwards compatibility broken. The following package versions are known to work on R 3.5.1:
- `readr` 1.1.1 (Note readr has a small bug that produces warnings, but doesn't affect results)
- `RCurl` 1.95-4.11
- `downloader` 0.4
- `knitr` 1.20
- `rmarkdown` 1.10
- `sp` 1.3-1
- `raster` 2.6-7
- `rasterVis` 0.45
- `dplyr` 0.7.6
- `tidyr` 0.8.1
- `rstan` 2.17.3
- `ggplot2` 3.0.0
- `cowplot` 0.9.3
- `pbmcapply` 1.2.5
- `Hmisc` 4.1-1
- `png` 0.1-7
- `gridBase` 0.4-7
- `pROC` 1.12.1
- `GGally` 1.4.0

To rebuild the manuscript, you must also install a version of [LaTex](https://www.latex-project.org/get/).
Run the following to generate all outputs (analysis, figures, table, manuscript):

```r
remake::make()
```


## Problems?
If you have any problems getting the workflow to run please create an issue at our [github repository](https://github.com/traitecoevo/mortality_bci) and we will endevour to remedy it ASAP.
