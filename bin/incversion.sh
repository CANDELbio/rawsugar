echo "Running $BASH_SOURCE"
DIRNAME=$(dirname $BASH_SOURCE)

if [ -z "$CIRCLE_BRANCH" ]
then
    CIRCLE_BRANCH=$(git rev-parse --abbrev-ref HEAD)
fi

# TODO OK, this is completely f'd It's either a circle build # or a git tag, should be one or the other
if [ -z "$CIRCLE_BUILD_NUM" ]
then
    CIRCLE_BUILD_NUM=$(git rev-parse --short HEAD)
fi

BUILD_VERSION=$(echo ${CIRCLE_BRANCH}.${CIRCLE_BUILD_NUM} | sed 's/\//-/g')

echo ${BUILD_VERSION} > ${DIRNAME}/../version
echo "Build version is $(cat $DIRNAME/../version)"
