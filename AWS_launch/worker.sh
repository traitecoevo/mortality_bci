## This isn't particularly nice, but should work to allow things to
## work on both the cluster (where things are hard coded to values
## that come from the clusterous yml) and on the local test system
## where the host/port come from the way that docker container linkage
## works.
if [ -z "$REDIS_HOST" ]; then
    REDIS_HOST=redis.marathon.mesos
    REDIS_PORT=31379
else
    REDIS_HOST=$(env | grep '^REDIS.*TCP_ADDR=' | sed  's/^.*=//')
    REDIS_PORT=$(env | grep '^REDIS.*TCP_PORT=' | sed  's/^.*=//')
fi

rrqueue_worker --redis-host ${REDIS_HOST} --redis-port ${REDIS_PORT} rrq
