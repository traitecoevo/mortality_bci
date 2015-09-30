FROM r-base

RUN apt-get update \
  && apt-get install -y --no-install-recommends -t unstable \
    ca-certificates \
    curl \
    gfortran \
    git \
    libcurl4-openssl-dev \
    libhiredis-dev \
    libicu-dev \
    libssh2-1-dev \
    libssl-dev \
    libxml2-dev \
    ssh \
    zlib1g-dev \
  && apt-get clean

RUN install2.r --error  \
    devtools \
    RcppRedis \
    rstan

RUN installGithub.r  \
    traitecoevo/callr \
    traitecoevo/dockertest \
    richfitz/storr \
    richfitz/remake \
    richfitz/ids \
    ropensci/RedisAPI \
    richfitz/RedisHeartbeat \
    gaborcsardi/progress \
    traitecoevo/rrqueue

RUN r -e 'dockertest:::copy_scripts_dir("/usr/local/bin")'

RUN r -e 'remake:::install_remake("/usr/local/bin")'

RUN r -e 'rrqueue:::install_scripts("/usr/local/bin")'

RUN mkdir /home/data \
  && echo "clone.sh /home/data" >> /root/.bashrc \
  && echo "system('clone.sh /home/data')" > /root/.Rprofile \
  && echo "system('clone.sh /home/data')" > /root/.littler.r

WORKDIR /home/data

CMD ["bash"]
