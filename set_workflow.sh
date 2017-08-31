#!/bin/bash

echo "deb https://cran.rstudio.com/bin/linux/ubuntu trusty/" >> /etc/apt/sources.list \
&& gpg --keyserver hkp://keyserver.ubuntu.com:80 --recv-k \
&& gpg -a --export E084DAB9 | apt-key add - \
&& apt-get update \
&& apt-get install -y --no-install-recommends \
r-base-dev=3.4.1-1trusty0 \
clang \
littler \
r-cran-littler \
&& echo 'options(repos = c(CRAN = "https://cran.rstudio.com/"), download.file.method = "libcurl")' >> /etc/R/Rprofile.site \
&& echo 'source("/etc/R/Rprofile.site")' >> /etc/littler.r \
&& ln -s /usr/share/doc/littler/examples/install.r /usr/local/bin/install.r \
&& ln -s /usr/share/doc/littler/examples/install2.r /usr/local/bin/install2.r \
&& ln -s /usr/share/doc/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
&& ln -s /usr/share/doc/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
&& install.r docopt \
&& apt-get clean \
&& apt-get autoremove \
&& rm -rf var/lib/apt/lists/*
  
  # Global site-wide config
  mkdir -p $HOME/.R/ \
&& echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function" >> $HOME/.R/Makevars \
&& echo "CXX=clang++ -ftemplate-depth-256" >> $HOME/.R/Makevars \
&& echo "CC=clang" >> $HOME/.R/Makevars

# Install other dependent R packages
install2.r -r "https://mran.revolutionanalytics.com/snapshot/2017-01-01/" --error \
  --deps "TRUE" \
  readr \
  downloader \
  dplyr \
  tidyr \
  rstan \
  R6 \
  yaml \
  digest \
  crayon \
  optparse

# Install remake
installGithub.r \
--deps "FALSE" \
richfitz/remake
richfitz/storr

# Remove unnecesarry tmp files
rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Clone shrub repository
git clone https://github.com/traitecoevo/mortality_bci

#chown -R jcamac mortality_bci (only required for BOAB)