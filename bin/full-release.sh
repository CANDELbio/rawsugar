export TIER=$1

bin/package.sh $TIER
bin/release.sh
