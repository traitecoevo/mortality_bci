## This is a nasty hack but lets this work locally, too.
REDIS_HOST=$(env | grep '^REDIS.*TCP_ADDR=' | sed  's/^.*=//')
REDIS_PORT=$(env | grep '^REDIS.*TCP_PORT=' | sed  's/^.*=//')
echo "Redis: ${REDIS_HOST}:${REDIS_PORT}"
rrqueue_worker --redis-host ${REDIS_HOST} --redis-port ${REDIS_PORT} rrq
