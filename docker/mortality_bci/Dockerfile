FROM r-base

RUN apt-get update \
  && apt-get install -y --no-install-recommends -t unstable \
    ca-certificates \
    curl \
    git \
    libcurl4-openssl-dev \
    libhiredis-dev \
    libicu-dev \
    libssh2-1-dev \
    libssl-dev \
    libxml2-dev \
    pandoc \
    ssh \
    zlib1g-dev \
  && apt-get clean

RUN rm /usr/local/bin/install2.r && \
  wget --no-check-certificate \
  https://raw.githubusercontent.com/cboettig/littler/master/examples/install2.r \
  -O /usr/local/bin/install2.r \
  && chmod +x /usr/local/bin/install2.r

RUN install2.r --error  \
    devtools \
    downloader \
    dplyr \
    inline \
    plyr \
    RcppEigen \
    RcppRedis \
    RCurl \
    rmarkdown

RUN \
  r -e 'devtools::install_version("BH", version="1.55.0-3")' && \
  MAKEVARS="-j1" install2.r --error \
  --repos=http://cran.rstudio.com \
  --repos=http://rstan.org/repo \
  rstan

RUN installGithub.r  \
    traitecoevo/callr \
    traitecoevo/dockertest \
    richfitz/storr \
    richfitz/remake \
    ropensci/RedisAPI \
    richfitz/RedisHeartbeat \
    gaborcsardi/progress \
    traitecoevo/rrqueue

RUN r -e 'dockertest:::copy_scripts_dir("/usr/local/bin")'

RUN r -e 'remake:::install_remake("/usr/local/bin")'

RUN r -e 'rrqueue:::install_scripts("/usr/local/bin")'

RUN mkdir /root/mortality_bci \
  && echo "clone.sh /root/mortality_bci" >> /root/.bashrc \
  && echo "system('clone.sh /root/mortality_bci')" > /root/.Rprofile \
  && echo "system('clone.sh /root/mortality_bci')" > /root/.littler.r

WORKDIR /root/mortality_bci

CMD ["bash"]
