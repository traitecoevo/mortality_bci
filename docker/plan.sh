## 0. preparation
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

## 1. Build the main image:
mkdir -p src
# clone_or_pull ../../rrqueue src/rrqueue
clone_or_pull https://github.com/traitecoevo/rrqueue src/rrqueue
docker build -t richfitz/mortality_bci -f mortality_bci/Dockerfile .

## 2. Build the worker image based on this:
docker build -t richfitz/mortality_bci_worker mortality_bci_worker

## 3. Clone the project
clone_or_pull ../ work

## 4. Configure to allow for 2 workers:
docker-compose scale redis=1 worker=2

## or just launch the container:
##   docker-compose up

## 5. Launch the controller:
docker run --link docker_redis_1:redis -v ${PWD}/work:/work -it richfitz/mortality_bci:latest R

# con <- RedisAPI::hiredis("redis")
# con$PING()

# con <- RedisAPI::hiredis("redis")
# obj <- rrqueue::queue("rrq", con=con)
# for (i in 1:100) {
#   obj$enqueue(sin(1))
# }

## Cleanup is not happening correctly, so workers are being lost.

## 6. Cleanup

## all, with confirmation:
##   docker-compose rm
## all workers, skipping y/n question:
##   docker-compose rm --force worker
