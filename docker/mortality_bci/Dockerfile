FROM r-base

RUN apt-get update \
  && apt-get install -y --no-install-recommends  \
    ca-certificates \
    curl \
    git \
    libcurl4-openssl-dev \
    libhiredis-dev \
    libxml2-dev \
    redis-server \
    ssh \
  && apt-get clean

# More recent copy of install2.r so that we can set repos more easily
RUN rm /usr/local/bin/install2.r && \
    wget --no-check-certificate \
    https://raw.githubusercontent.com/cboettig/littler/master/examples/install2.r\
    -O /usr/local/bin/install2.r \
  && chmod +x /usr/local/bin/install2.r

RUN install2.r --error  \
    RCurl \
    RcppEigen \
    RcppRedis \
    devtools \
    downloader \
    dplyr \
    inline \
    rmarkdown

RUN MAKEVARS="-j1" install2.r --error  \
    --repos=http://cran.rstudio.com  \
    --repos=http://rstan.org/repo \
    rstan

RUN installGithub.r  \
    richfitz/remake \
    richfitz/remoji \
    richfitz/rfiglet \
    richfitz/RedisAPI \
    richfitz/RedisHeartbeat \
    traitecoevo/rrqueue

CMD ["bash"]
