# this does the full operation of putting the current working dir onto the selected server.
echo "Running $BASH_SOURCE"

export TIER=$1
source deploy/config/$TIER-config.sh

bin/package.sh $TIER
bin/build.sh $TIER
deploy/kubernetes/deploy-app-server.sh $TIER


