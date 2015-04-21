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

function boot2docker_init {
    if [ `uname` == "Darwin" ]; then
        if [ -z $DOCKER_HOST ]; then
            $(boot2docker shellinit 2>/dev/null)
        fi
    fi
}
