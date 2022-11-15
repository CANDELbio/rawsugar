# Currently only used for dev to set up local env
# parallels for real are in bin/build.sh and deploy/kubernetes/deploy-app-server.sh (and probably in .circleci/config.yml

export TIER=$1
source deploy/config/$TIER-config.sh
gcloud config set core/project $GCS_PROJECT
gcloud container clusters get-credentials $CLUSTER_NAME
