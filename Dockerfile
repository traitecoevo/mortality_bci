FROM r-base:3.4.0

# Install latex, git and clang then clean up tmp files
RUN    apt-get update \
    && apt-get install -y --no-install-recommends \
         libcurl4-openssl-dev \
         git \
         clang \
    && apt-get clean \
    && apt-get autoremove \
    && rm -rf var/lib/apt/lists/*

# Global site-wide config
RUN mkdir -p $HOME/.R/ \
    && echo "\nCXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function\n" >> $HOME/.R/Makevars \
    && echo "\nCXX=clang++ -ftemplate-depth-256\n" >> $HOME/.R/Makevars \
    && echo "CC=clang\n" >> $HOME/.R/Makevars

# Install other dependent R packages
RUN install2.r -r "https://mran.revolutionanalytics.com/snapshot/2017-01-01/" --error \
    --deps "TRUE" \
    readr RCurl downloader rmarkdown sp viridisLite raster rasterVis latticeExtra dplyr tidyr rstan knitr ggplot2 cowplot

# Install remake
RUN installGithub.r \
    --deps "TRUE" \
    richfitz/remake

# Remove unnecesarry tmp files
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Install remake on command line
RUN r -e 'remake:::install_remake("/usr/local/bin")'

RUN mkdir /home/data \
  && echo "clone.sh /home/data" >> /root/.bashrc \
  && echo "system('clone.sh /home/data')" > /root/.Rprofile \
  && echo "system('clone.sh /home/data')" > /root/.littler.r

WORKDIR /home/data

CMD ["bash"]
