#!/bin/sh
docker ps | awk '{print $1}' | tail -n +2 | xargs docker kill
