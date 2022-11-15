# rawsugar

Rawsugar is PICI՚s tool for raw data ingestion, storage, and retrieval. It is a successor to [PDS](https://github.com/ParkerICI/pici-data-storage).

It includes a command-line interface (CLI) and a web interface. Please see the  [user guide](https://github.com/ParkerICI/rawsugar/blob/master/doc/user-guide.org) for details about its design.

# Current state of this repo

This repo has been forked from the original PICI repo. 

There's a major change in the build, which is only half-done: rather than use cljsjs for external javascript libraries, Rawsugar now gets them from npm. This change frees us from having to build and maintain cljsjs packages, but it also requires that the cljs application be built using shadow-cljs, which replaces figwheel-main.

In the current state, this is halfway done, and needs to be cleaned up, but you can build an operating cljs client with shdow-cljs (see below).

# Use

## CLI

    lein run <cmd> 

See the user guide for details

## Web

You can start a local server with

    lein run insecure-server
	
TODO describe required configuration


# Development 

The rest of this `README` is aimed at developers and if you are just trying to use Rawsuger you can ignore it.

For development work, you will need to use a non-production Datomic instance. You can either use the dev/staging instance in the cloud, or run a local version. Unless you have a good reason, I recommend using the  cloud instance.

## Local Dataomic
	
### Download Datomic Pro Starter Edition

From https://www.datomic.com/get-datomic.html . Follow the instructions to obtain a license key, which will be emailed to you.

We՚ll assume you have the datomic installation under the `rawsugar` directory for the purposes of these instructions. Also you may get a later version of Datomic than mentioned below, that should work fine, just make appropriate substitutions in pathnames.

### Credentials

Create `resources/credentials/dev-transactor.properties` by editing the existing template file  `credentials/dev-transactor.properties.template`  and adding your license key at the designated place.

### Run Datomic transactor and peer server:

Note: Datomic doesn't run in late versions of Java, hence the fiddling below

In separate shells, run:

```
cd <datomic-pro-VERSION>
export JAVA_HOME=`/usr/libexec/java_home -v 11.0.3`
bin/transactor credentials/dev-transactor.properties

```
and

```
cd <datomic-pro-VERSION>
export JAVA_HOME=`/usr/libexec/java_home -v 11.0.3`
bin/run -m datomic.peer-server -h localhost -p 8999 -a myaccesskey,mysecret -d rawsugar,datomic:dev://localhost:4334/rawsugar
```

### Create the database

Note: this step should only have to be run once per installation.

```
datomic-pro-0.9.5786/bin/repl
user=> (require 'datomic.api)
user=> (datomic.api/create-database "datomic:dev://localhost:4334/rawsugar")
```

### Add the schema

```
lein with-profile test run initialize-schema
```


### Run Datomic peer server

In a separate shell, run:
```
datomic-pro-0.9.5786/bin/run -m datomic.peer-server -h localhost -p 8998 -a myaccesskey,mysecret -d rawsugar,datomic:dev://localhost:4334/rawsugar
```

### Initialize database

Note: this step should only have to be run once per installation.

```
lein run initialize
```

## Run application

Now finally you can issue commands.

```
lein run list-projects (etc)
```
or for more efficiency, build an uberjar:
```
lein uberjar

```
and then you can issue commands like this:

```
java -jar target/rawsugar-0.1.0-SNAPSHOT-standalone.jar list-projects

```

## shadow-cljs 

The CLJS app is now built with shadow-cljs, see https://shadow-cljs.github.io/docs/UsersGuide.html

### One-time setup

TODO install npm, shadow-cljs	

    npm install

### Building app

    shadow-cljs compile app
	
To develop with automatic reload (note: doesn't work due to some dependency conflict thing, but it should. TODO)

    shadow-cljs watch app


