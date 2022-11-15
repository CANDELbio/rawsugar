# Fill out and apply a k8 template

export TEMPLATE=$1
export YAML=${TEMPLATE%.*}

# envsubst is part of gettext package. If this doesn't work on Mac, try 'brew install gettext'
# Unfortunately it has no option to throw error if a var is undefined...
/usr/local/opt/gettext/bin/envsubst < $TEMPLATE > $YAML
kubectl apply -f $YAML

