# run from repo top level
# First arg is tier, currently either "prod" or "test"

export TIER=$1
echo "Running $BASH_SOURCE"
DIRNAME=$(dirname $BASH_SOURCE)

source deploy/config/$TIER-config.sh

export VERSION=$(cat ${DIRNAME}/../version)
echo Version $VERSION

# make sure we are talking to the right project
gcloud config set core/project $GCS_PROJECT

docker build -t gcr.io/$GCS_PROJECT/rawsugar-app-server:$VERSION .
docker push gcr.io/$GCS_PROJECT/rawsugar-app-server:$VERSION 

# see deploy/kubernetes/deploy-app-server.sh for next step
