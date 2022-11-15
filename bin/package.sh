#!/bin/bash
echo "Running $BASH_SOURCE"
DIRNAME=$(dirname $BASH_SOURCE)

bin/incversion.sh
export TIER=$1
source deploy/config/$TIER-config.sh

export VERSION=$(cat ${DIRNAME}/../version)

echo Version $VERSION
lein set-version $VERSION
#git tag v$VERSION

# Build uberjar 
lein with-profile +$TIER clean
bin/compile-user-guide.sh
lein with-profile +$TIER uberjar

# Prepare package directory
git clean -f package
cp target/rawsugar-standalone.jar package

# TODO doesn't need to be done in Circle build
# Create zip file
rm rawsugar*.zip
zip -rj rawsugar-${TIER}-${VERSION}.zip package


