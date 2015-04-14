mkdir -p src

function clone_or_pull {
    SRC="$1"
    DEST="$2"
    if [ -d ${DEST} ]; then
        echo "pulling ${DEST} (from ${SRC})"
        git -C ${DEST} pull --ff-only
    else
        echo "cloning ${SRC} to ${DEST}"
        git clone ${SRC} ${DEST}
    fi
}  

# clone_or_pull ../../rrqueue src/rrqueue
clone_or_pull ../ work
WORK=$(pwd)/work
QUEUE=rrq

docker build -t richfitz/mortality_bci -f mortality_bci/Dockerfile .
docker build -t richfitz/mortality_bci_worker mortality_bci_worker

## The database, if it's not up already
if [ $(docker inspect -f {{.State.Running}} redis) == "false" ]; then
    docker run --name redis -d redis:latest
fi

## Workers:
docker run --link redis:redis -v ${WORK}:/work -it mortality_bci_worker worker1.log ${QUEUE} --redis-host redis
docker run --link redis:redis -v ${WORK}:/work -it mortality_bci_worker worker2.log ${QUEUE} --redis-host redis

docker run --link redis:redis -v ${WORK}:/work -it mortality_bci

# Then in R:
# con <- RedisAPI::hiredis("redis")
# obj <- rrqueue::queue("rrq", con=con)
# for (i in 1:100) {
#   obj$enqueue(sin(1))
# }
