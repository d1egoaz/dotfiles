#!/bin/sh

(set -x; scalafmt -f $1 --config=/Users/diegoa/.scalafmt.conf -i --exclude=Test,test)
