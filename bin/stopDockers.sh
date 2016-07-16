#!/bin/bash
/usr/local/bin/docker ps | awk '{print $1}' | grep -v CON | xargs docker stop
