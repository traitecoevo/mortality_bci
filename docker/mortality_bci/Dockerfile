FROM r-base

RUN apt-get update \
  && apt-get install -y --no-install-recommends  \
    ca-certificates \
    curl \
    git \
    libcurl4-openssl-dev \
    libhiredis-dev \
    pandoc \
    ssh \
  && apt-get clean

RUN rm /usr/local/bin/install2.r && \
  wget --no-check-certificate \
  https://raw.githubusercontent.com/cboettig/littler/master/examples/install2.r \
  -O /usr/local/bin/install2.r \
  && chmod +x /usr/local/bin/install2.r

RUN install2.r --error  \
    crayon \
    devtools \
    digest \
    docopt \
    downloader \
    dplyr \
    inline \
    R6 \
    RcppEigen \
    RcppRedis \
    RCurl \
    rmarkdown

RUN MAKEVARS="-j1" install2.r --error \
  --repos=http://cran.rstudio.com \
  --repos=http://rstan.org/repo \
  rstan

RUN installGithub.r  \
    richfitz/remake \
    ropensci/RedisAPI \
    richfitz/RedisHeartbeat \
    gaborcsardi/progress \
    richfitz/dockertest

RUN r -e 'dockertest:::copy_scripts_dir("/usr/local/bin")'

COPY local /local

RUN R CMD INSTALL /local/rrqueue \
  && rm -rf /local

RUN r -e 'remake:::install_remake("/usr/local/bin")'

RUN r -e 'rrqueue:::install_scripts("/usr/local/bin")'

WORKDIR /root/mortality_bci

CMD ["bash"]
