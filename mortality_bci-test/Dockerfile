FROM r-base

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    git \
    libcurl4-openssl-dev \
    libicu-dev \
    libssh2-1-dev \
    libssl-dev \
    libxml2-dev \
    ssh \
    zlib1g-dev \
  && apt-get clean

RUN install2.r --error devtools

RUN installGithub.r  \
    traitecoevo/callr \
    traitecoevo/dockertest

RUN r -e 'dockertest:::copy_scripts_dir("/usr/local/bin")'

RUN mkdir /root/mortality_bci \
  && echo "clone.sh /root/mortality_bci" >> /root/.bashrc \
  && echo "system('clone.sh /root/mortality_bci')" > /root/.Rprofile \
  && echo "system('clone.sh /root/mortality_bci')" > /root/.littler.r

WORKDIR /root/mortality_bci

CMD ["bash"]
