#!/bin/bash
docker ps -a | awk '{print $1}' | grep -v CON | xargs docker rm
