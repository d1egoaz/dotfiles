#!/bin/sh
docker ps | awk '{print $1}' | tail -1 | xargs docker kill
