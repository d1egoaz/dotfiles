#!/bin/sh

(set -x; scalafmt -f $1 --config=/Users/diegoa/.scalafmt2.conf -i)
