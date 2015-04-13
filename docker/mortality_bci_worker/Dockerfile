FROM mortality_bci
WORKDIR /work
RUN Rscript -e "rrqueue:::install_rrqueue_worker('/usr/bin')"
CMD ["rrqueue_worker", "--redis-host", "redis"]
